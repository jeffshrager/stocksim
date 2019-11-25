;; (load (compile-file "stocksim.lisp"))
(declaim (optimize (debug 3)))

(defun string-split (string &key (delimiter #\space) (convert-num-values? nil))
  "Split string into substrings delimited by delimiter"
  (let ((substrings '())
        (length (length string))
        (last 0))
    (flet ((add-substring 
	    (i)
	    (push (subseq string last i)
		  substrings)))
	  (dotimes (i length)
	    (when (eq (char string i) delimiter)
	      (add-substring i)
	      (setq last (1+ i))))
	  (add-substring length)
	  (let ((substrings (nreverse substrings)))
	    (if convert-num-values?
		(loop for string in substrings
		      as v = (ignore-errors (read-from-string string))
		      if (numberp v)
		      collect v
		      else 
		      collect string)
	      substrings)))))

(defun load-stocks ()
  (with-open-file 
   (i "NGTTKYK3.csv")
   (loop for line = (read-line i nil nil)
	 until (null line)
	 collect (string-split line :delimiter #\, :convert-num-values? t))))

(defparameter *bank* 10000.00) ;; Start out with 10k$
(defparameter *irate* 3.0) ;; Interest rate in %
(defparameter *data* (load-stocks))
(defparameter *sym.shares* (loop for sym in (cdr (pop *data*)) collect (cons (read-from-string sym) 0)))
(defparameter *vbank* *bank*)
(defparameter *maxrecn* (length *data*))

(defun run ()
  (prog (wkn val rec $ sym skip)
	(setf wkn 0)
	loop 
	(setf $ 0.0 rec (nth wkn *data*) skip 1)
	(when (null rec) 
	  (format t "~%~%*****************~%~%We're up to today! You can't go any father. We'll hang out at the last datapoint.~%You can use reset (r r r) to reset to the beginning of time.~%~%")
	  (setf rec (car (last *data*))))
	(format t "~%~%================= ~a =================~%" (pop rec))
	(format t "You have $~,2f in the bank (@~,2f%), and own:~%" *bank* *irate*)
	(loop for (sym . shares) in *sym.shares*
	      as sval in rec
	      as s$ = (* sval shares)
	      unless (= 0.0 s$)
	      do 
	      (format t "  ~a shares of ~a @ $~,2f/share = $~,2f~%" 
		      shares sym sval s$)
	      (incf $ s$))
	(format t "~%Your total stock value is $~a~%Your net worth is **** $~,2f ****~%" $ (+ *bank* $))
	(format t "        (If you'd just kept your money in the bank @ ~,2f%, it would now be worth: $~,2f)~%" *irate* *vbank*)
	(format t "~%You could buy:~%")
	(loop for (sym . shares) in *sym.shares*
	      as sval in rec
	      as s$ = (* sval shares)
	      when (= 0.0 s$)
	      do 
	      (format t "  ~a shares of ~a @ $~,2f/share = $~,2f~%" 
		      "<tbd>" sym sval "<tbd>")
	      )
	(format t "~%~%(b)uy sym #, (s)ell sym #, (r)eset, (f)wd #wks(default=1), (q)uit:~%")
	(multiple-value-bind
	 (cmd arg1 arg2)
	 (get-command)
	 (unless 
	      (ignore-errors
	       (case cmd
		     ((f fwd ff) 
		      (print (list 'forward arg1 arg2))
		      (if (< (+ arg1 wkn) *maxrecn*)
			  (setf skip (or arg1 1))
			(format t "ERROR: Can't skip forward more than ~a weeks." (- *maxrecn* wkn))))
		     ((b buy) (print (list 'buy arg1 arg2))
		      (if (numberp arg2) 
			  (if (>= *bank* (* (curvalof arg1 rec) arg2))
			      (progn
				(incf (cdr (assoc arg1 *sym.shares*)) arg2)
				(decf *bank* (* (curvalof arg1 rec) arg2)))
			    (format t "*** CAN'T *** Buying ~a shares of ~a would cost $~,2f~%" arg1 arg1 (* (curvalof arg1 rec) arg2)))
			(format t "I was expecting a number as the second arg. Maybe you reveresed the args?~%")))
		     ((s sell) (print (list 'sell arg1 arg2))
		      (if (numberp arg2) 
			  (if (<= arg2 (mysharesof arg1))
			      (progn
				(decf (cdr (assoc arg1 *sym.shares*)) arg2)
				(incf *bank* (* (curvalof arg1 rec) arg2)))
			    (format t "You only have ~a shares of ~a.~%" (mysharesof sym) sym))
			(format t "I was expecting a number as the second arg. Maybe you reveresed the args?~%"))
		      )
		     ((r reset) 
		      (setf *bank* 10000 *vbank* *bank* wkn 0)
		      (loop for elt in *sym.shares* do (setf (cdr elt) 0.0)))
		     ((q quit) (return nil))
		     (t (print 'hunh?)))
	       t) 
	    (format t "~%~%????????? Something went wrong !!!!!!!!!!~%~%")
	    ))
	;; Compounded daily, but we're skipping by weeks!
	(let* ((k 365.0) ;; We're compounding daily
	       (kt (* k (/ (* skip 7.0d0) 365.0d0))) ;; Running 7 days at a time (times the skip)
	       (r/k (/ (/ *irate* 100.0d0) k))
	       (1+r/k (+ 1.0d0 r/k))
	       (1+r/k^kt (expt 1+r/k kt))
	       (final (* *bank* 1+r/k^kt)))
	  ;;(print `(k ,k kt ,kt r/k ,r/k 1+r/k ,1+r/k 1+r/k^kt ,1+r/k^kt final ,final))
	  (setf *bank* final)
	  (setf *vbank* (* *vbank* 1+r/k^kt))
	  )
	(incf wkn skip)
	(go loop)))

;;; Commands are like:
;;;    buy 10 goog
;;;    b goog 10
;;;    reset
;;;    quit
;;;
;;; This always returns three values: the command (a symbol), 
;;; and then in some cases a symbol and a number of shares.

(defun get-command ()
  (let* ((cmd (string-trim " " (fast-substitute (read-line t nil nil))))
	 (p (mapcar #'(lambda (w) (string-trim ",./;: " w)) (string-split cmd :delimiter #\space)))
	 (cmd (read-from-string (pop p)))
	 (arg1 (when p (read-from-string (pop p))))
	 (arg2 (when p (read-from-string (pop p))))
	 )
    (print (list cmd arg1 arg2))
    (values cmd arg1 arg2)))

(defun fast-substitute (string)
  (loop for c across string
	as p from 0 by 1
	when (or (char-equal c #\.)
		 (char-equal c #\,)
		 (char-equal c #\()
		 (char-equal c #\))
		 (char-equal c #\')
		 (char-equal c #\#)
		 (char-equal c #\tab)
		 (char-equal c #\:)
		 (char-equal c #\!)
		 (char-equal c #\~)
		 (char-equal c #\>)
		 (char-equal c #\<)
		 (char-equal c #\/)
		 (char-equal c #\%)
		 (char-equal c #\&)
		 (char-equal c #\")
		 )
	do (setf (aref string p) #\space))
  string)

(defun mysharesof (sym)
  (cdr (assoc sym *sym.shares*)))

(defun curvalof (sym rec)
  (loop for (tsym . nil) in *sym.shares*
	as val in rec
	when (eq sym tsym)
	do (return val)))

(run)
