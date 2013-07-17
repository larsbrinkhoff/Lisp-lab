;;;; Funcall with optional arguments (not &optional parameters).

;;; This is the flip side of functions with optional parameters, i.e.

;; If we have a function defined like this
(defun foo (x &optional y) ...)
;; we can call it with two different argument lists.
(dolist (args (list '(1) '(2 3)))
  (apply #'foo args))

;;; Now, we want this to work:

;; We have two functions defined like this
(defun bar (x) ...)
(defun baz (x y) ... )
;; and we want to be able to call them both with the same argument list:
(dolist (fn (list #'bar #'baz))
  (foptioncall fn 1 &optional 2))

;;; Possible implementation:

(defmacro foptioncall (fn &rest args)
  (let* ((optional (member '&optional args))
	 (required (ldiff args optional))
	 (optional (rest optional)))
    (with-gensyms (opt)
      `(let ((,opt (copy-list ',optional)))
	 (tagbody
	  again
	    (handler-bind ((program-error (lambda (x)
					    (unless opt
					      (error "..."))
					    (setq ,opt (nbutlast ,opt))
					    (go again))))
	      (apply #'foo ,@required ,opt)))))))
