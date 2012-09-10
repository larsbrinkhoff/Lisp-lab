(defpackage :se.brinkhoff.time-syntax
  (:nicknames :time-syntax)
  (:use :cl)
  (:shadow :time)
  (:export :enable-#T-syntax :enable-number-syntax))

(in-package :se.brinkhoff.time-syntax)

(defvar *#T-readtable* (copy-readtable))

(defvar *#T-pprint-dispatch* (copy-pprint-dispatch))

(defclass time ()
  (hours
   minutes
   seconds))

(defvar *whitespace*
  (let ((list '(#\Space #\Newline)))
    (dolist (x '("#\\Return" "#\\Linefeed" "#\\Tab"))
      (let ((ch (ignore-errors (read-from-string x))))
	(when ch
	  (pushnew ch list))))
    list))

(defun whitespacep (ch)
  (member ch '#.*whitespace*))

(defun \#T-reader (stream char arg)
  (declare (ignore char arg))
  (let ((minutes 0.0)
	(seconds 0))
    (tagbody
     minutes
       (let ((ch (read-char stream nil)))
	 (cond ((null ch)		(go done))
	       ((whitespacep ch)	(go done))
	       ((eql ch #\:)		(go seconds))
	       ((digit-char-p ch)	(setq minutes (+ (* 10 minutes)
							 (digit-char-p ch))))
	       (t			(error "foo"))))
       (go minutes)
     seconds
       (let ((ch (read-char stream nil)))
	 (cond ((null ch)		(go done))
	       ((whitespacep ch)	(go done))
	       ((digit-char-p ch)	(setq seconds (+ (* 10 seconds)
							 (digit-char-p ch))))
	       (t			(error "foo"))))
       (go seconds)
     done)
    (return-from \#T-reader (+ minutes (/ seconds 60)))))

(defun min->dec (minutes)
  (ecase (count #\: minutes)
    (0 (values (read-from-string minutes)))
    (1 (let ((colon (position #\: minutes)))
	 (+ (read-from-string minutes t nil :end colon)
	    (/ (read-from-string minutes t nil :start (1+ colon)) 60.0))))
    (2 (let ((colon (position #\: minutes)))
	 (+ (* 60 (read-from-string minutes t nil :end colon))
	    (min->dec (subseq minutes (1+ colon))))))))

(defun whitespacep (char)
  (member char '(#\Space #\Return #\Newline #\Tab)))

(defun end-of-token-p (char)
  (or (null char)
      (multiple-value-bind (x y) (get-macro-character char)
	(and (functionp x) (null y)))
      (whitespacep char)))

(defun token-char (stream)
  (if (end-of-token-p (peek-char nil stream nil))
      nil
      (read-char stream)))

(defun read-token (stream &optional prefix-char)
  (let ((token (loop for char = (token-char stream)
		     while char
		     collect char)))
    (when prefix-char
      (push prefix-char token))
    (coerce token 'string)))
  
(defun time-string-p (string)
  (and (find #\: string)
       (every (lambda (ch) (or (digit-char-p ch) (member ch '(#\: #\.))))
	      string)))

(defun number-reader (stream char)
  (let ((token (read-token stream char)))
    (if (time-string-p token)
	(min->dec token)
	(let ((*readtable* (copy-readtable nil))) ; with-standard-io-syntax
	  (read-from-string token)))))

(defun \#T-printer (stream object)
  (check-type object time)
  (write-string "#T" stream)
  (with-slots (hours minutes seconds) object
    (when hours
      (format stream "~D:" hours))
    (when minutes
      (format stream "~D:" minutes))
    (format stream "~F" seconds))
  nil)

(defun enable-#T-syntax (&key (readtable *readtable*)
			      (pprint-dispatch *print-pprint-dispatch*))
  (set-dispatch-macro-character #\# #\T '\#T-reader readtable)
  (set-pprint-dispatch 'time '\#T-printer 10 pprint-dispatch)
  nil)

(defun enable-number-syntax (&key (readtable *readtable*))
  (dotimes (i 10)
    (set-macro-character (digit-char i) #'number-reader t readtable)))

(enable-#T-syntax :readtable *#T-readtable*
		  :pprint-dispatch *#T-pprint-dispatch*)
