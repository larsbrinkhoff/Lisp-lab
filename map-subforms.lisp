(in-package :cl-user)

;;; Operators using environment objects:
;;; defmacro, macroexpand, macroexpand-1, macro-function, *macroexpand-hook*,
;;; macrolet, define-compiler-macro, compiler-macro-function,
;;; define-setf-expander, defsetf, get-setf-expansion,
;;; constantp, make-load-form, make-load-form-saving-slots,
;;; deftype, typep, subtypep, upgraded-array-element-type,
;;; upgraded-complex-part-type, find-class

(defun compilation-error (form)
  (handler-bind ((warning #'muffle-warning)
		 (error (lambda (c) (return-from compilation-error c))))
    (let ((*standard-output* (make-broadcast-stream))
	  (*error-output* (make-broadcast-stream)))
      (nth-value 2 (compile nil `(lambda () ,form))))))

(defun map-unknown-subforms (fn form)
  (when (atom form)
    (return-from map-unknown-subforms form))
  (labels ((walk (subforms)
	     (unless (endp subforms)
	       (let* ((sym (gensym))
		      (sym1 (gensym))
		      (x (first subforms))
		      (unexpanded t)
		      (result x)
		      (fn (lambda (x)
			    (setq unexpanded nil
				  result (funcall fn x))))
		      (compile-form
		       `(macrolet
			 ((,sym (x)
			   (let ((*standard-output* ,*standard-output*)
				 (*error-output* ,*error-output*))
			     (funcall ,fn x))
			   x))
			 ,form)))
		 (if (symbolp x)
		     (setf (first subforms) sym1
			   compile-form
			   `(symbol-macrolet ((,sym1 (,sym ,x)))
			     ,compile-form))
		     (setf (first subforms) `(,sym ,x)))
		 (when (compilation-error compile-form)
		   (setq result x
			 unexpanded t))
		 (setf (first subforms) result)
		 (when (and unexpanded (consp result))
		   (walk result))
		 (walk (rest subforms))))))
    (walk (rest form))
    form))

(defun map-unknown-subforms (fn form)
  (when (atom form)
    (return-from map-unknown-subforms form))
  (labels ((walk (subforms)
	     (unless (endp subforms)
	       (let* ((sym (gensym))
		      (sym1 (gensym))
		      (x (first subforms))
		      (result #1='#:unexpanded)
		      (fn (lambda (x) (setq result (funcall fn x))))
		      (compile-form
		       `(macrolet
			 ((,sym (x)
			   (let ((*standard-output* ,*standard-output*)
				 (*error-output* ,*error-output*))
			     (funcall ,fn x))
			   x))
			 ,form)))
		 (if (symbolp x)
		     (setf (first subforms) sym1
			   compile-form
			   `(symbol-macrolet ((,sym1 (,sym ,x)))
			     ,compile-form))
		     (setf (first subforms) `(,sym ,x)))
		 (when (prog1 (or (compilation-error compile-form)
				  (eq result #1#))
			 (setf (first subforms) x))
		   (setq result (if (consp x) (walk x) x)))
		 (cons result (walk (rest subforms)))))))
    (cons (first form) (walk (rest form)))))

(defmacro destructuring-typecase (form &body clauses)
  (let ((x (gensym)))
    `(let ((,x ,form))
       (typecase ,x
	 ,@(mapcar (lambda (clause)
		     (destructuring-bind (type lambda-list &body body) clause
		       `(,type
			 (destructuring-bind (,lambda-list) (list ,x)
			   ,@body))))
	       clauses)))))

#+swank
(eval-when (:execute :load-toplevel)
  (swank::send-to-emacs
   '(:indentation-update (("destructuring-typecase"
			   (4 "&rest" ("&whole" 2 4 "&rest" 2)))))))

#|
;;; This is Emacs Lisp:
(defun indent-destructuring-typecase (path state indent-point
				      sexp-column normal-indent)
  (case (car path)
    (1		(+ sexp-column 4))
    (t		(cond ((null (cdr path))	(+ sexp-column 2))
		      ((cddr path)		normal-indent)
		      ((eq (cadr path) 1)	(+ sexp-column 4))
		      (t		(+ sexp-column 2))))))
|#

(deftype list-of (&rest types)
  (typecase types
    (null		'null)
    (cons		`(cons ,(car types) (list-of ,@(cdr types))))
    (atom		types)))

(deftype let-form () '(list-of (eql let) list . list))
(deftype let*-form () '(list-of (eql let*) list . list))
(deftype flet-form () '(list-of (eql flet) list . list))
(deftype labels-form () '(list-of (eql labels) list . list))
(deftype setq-form () '(list-of (eql setq) . list))
(deftype lambda-expr () '(list-of (eql lambda) list . list))
(deftype lambda-form () '(list-of lambda-expr . list))
(deftype function-name () '(or symbol (list-of (eql setf) symbol)))
(deftype function-form (&optional (arg '(or function-name lambda-expr)))
  `(list-of (eql function) ,arg))
(deftype special-form ()
  '(cons (and symbol (satisfies special-operator-p)) list))
(deftype macro-form ()
  '(cons (and symbol (satisfies macro-function)) t)) ;Allow dotted lists.
(deftype macrolet-form () '(list-of (eql macrolet) list . list))
(deftype symbol-macrolet-form () '(list-of (eql symbol-macrolet) list . list))
(deftype load-time-value-form () '(cons (eql load-time-value)
				        (cons t (or null (cons t null)))))
(deftype locally-form () '(list-of (eql locally) . list))
(deftype tagbody-form () '(list-of (eql tagbody) . list))

(deftype let/*-form () '(or let-form let*-form))

(deftype function-binding-form (&rest operators)
  `(list-of (member ,@(or operators '(flet labels macrolet))) list . list))

(deftype variable-binding-form (&rest operators)
  (let ((op-forms '((let . let-form)
		    (let* . let*-form)
		    (lambda . lambda-form)
		    (function . (function-form lambda-expr)))))
    `(or ,@(mapcar (lambda (op) (cdr (assoc op op-forms)))
		   (or operators '(let let* lambda function))))))

(defun let-variables (bindings)
  (mapcar (lambda (b)
	    (etypecase b
	      (symbol				b)
	      ((cons symbol)			(car b))))
	  bindings))

(defun lambda-list-variables (lambda-list)
  (mapcan (lambda (x)
	    (append
	     (etypecase x
	       (symbol				(list x))
	       ((cons symbol)			(list (car x)))
	       ((cons (list-of symbol symbol))	(list (cadar x))))
	     (typecase x
	       ((list-of t t symbol)		(list (third x))))))
	  (remove-if (lambda (x) (member x lambda-list-keywords))
		     lambda-list)))

(defun binding-form-variables (form)
  (check-type form variable-binding-form)
  (destructuring-typecase form
    (let/*-form (let bindings &rest body)
      (declare (ignore let body))
      (let-variables bindings))
    (lambda-form ((lambda lambda-list &rest body) &rest forms)
      (declare (ignore lambda body forms))
      (lambda-list-variables lambda-list))
    ((function-form lambda-expr) #'(lambda lambda-list &rest body)
      (declare (ignore function lambda body))
      (lambda-list-variables lambda-list))
    (symbol-macrolet-form (symbol-macrolet bindings &rest body)
      (declare (ignore symbol-macrolet body))
      (mapcar #'first bindings))))

(defun binding-form-functions (form)
  (check-type form function-binding-form)
  (destructuring-bind (op bindings . body) form
    (declare (ignore op body))
    (mapcar #'first bindings)))

(defmacro with-gensyms (vars &body body)
  `(let ,(mapcar (lambda (var) `(,var (gensym))) vars)
     ,@body))

(defun form-body (form)
  (check-type form (not macro-form))
  (typecase form
    ((cons (member locally progn tagbody))
     (cdr form))
    ((cons (member block catch eval-when let let* multiple-value-prog1
		   symbol-macrolet unwind-protect))
     (cddr form))
    (function-binding-form
     (values (cddr form) (mapcar (lambda (b) (nthcdr 2 b)) (second form))))
    ((function-form lambda-expr)
     (cddadr form))
    (lambda-form
     (cddar form))
    (t
     (values))))

(defun (setf form-body) (body form &optional env)
  (declare (ignore env))
  (check-type form (not macro-form))
  (typecase form
    ((cons (member locally progn tagbody))
     (setf (cdr form) body))
    ((cons (member block catch eval-when flet labels let let* macrolet
		   multiple-value-prog1 symbol-macrolet unwind-protect))
     (setf (cddr form) body))
    (lambda-form
     (setf (cddar form) body))
    ((function-form lambda-expr)
     (setf (cddadr form) body))
    (special-form
     (error "Unknown special form: ~A" form))
    (t
     body)))

(defun quote-tree (x &optional (unquote (list)))
  (cond
    ((atom x)
     `(quote ,x))
    ((eq (car x) unquote)
     x)
    (t		
     `(cons ,(quote-tree (car x) unquote) ,(quote-tree (cdr x) unquote)))))
(defun quote-tree (x &optional (unquote (list)))
  (cond
    ((atom x)
     `',x)
    ((eq (car x) unquote)
     x)
    (t		
     ``(,,(quote-tree (car x) unquote) . ,,(quote-tree (cdr x) unquote)))))
(defun simplify-quote (x)
  (labels ((constant-or-quoted-p (x)
	     (typep x '(or (and atom (satisfies constantp))
			   (list-of (eql quote) t))))
	   (unquote (x)
	     (if (typep x '(list-of (eql quote) t))
		 (second x)
		 x))
	   (simplify-list (elts)
	     (if (every #'constant-or-quoted-p elts)
		 `(quote (,@(mapcar #'unquote elts)))
		 `(list ,@elts))))
    (destructuring-typecase x
      ((list-of (eql quote) t) (_ y)
	(declare (ignore _))
	(if (and (atom y) (constantp y)) y x))
      ((list-of (eql cons) t t) (_ car cdr)
	(declare (ignore _))
	(let ((car (simplify-quote car))
	      (cdr (simplify-quote cdr)))
	  (destructuring-typecase cdr
	    (null _	(declare (ignore _))
			(simplify-list (list car)))
	    ((list-of (eql quote) list) (_ list)
			(declare (ignore _))
			(if (constant-or-quoted-p car)
			    `(quote (,(unquote car) ,@list))
			    `(cons ,car ,cdr)))
	   ((list-of (eql list) . list) (_ . list)
			(declare (ignore _))
			(simplify-list (cons car list)))
	   (t _		(declare (ignore _))
			`(cons ,car ,cdr)))))
      (t _
	(declare (ignore _))
	x))))

(deftype declaration-expr () '(cons (eql declare) list))

(deftype standard-declaration-identifier ()
  '(member declaration dymnamic-extent ftype function ignore inline
    notinline optimize special type))

(defvar *doc-allowed*)
(defun doc-allowed (x) (declare (ignore x)) *doc-allowed*)
(deftype doc-string () '(and string (satisfies doc-allowed)))

(defun parse-body (body &optional *doc-allowed*)
  (destructuring-typecase body
    ((cons declaration-expr list) (decl . body)
      (multiple-value-bind (forms decls doc) (parse-body body *doc-allowed*)
	(values forms (cons decl decls) doc)))
    ((cons doc-string cons) (doc . body)
      (multiple-value-bind (forms decls) (parse-body body)
	(values forms decls doc)))
    (t body
      (values body nil nil))))

(deftype operator-with-forms ()
  '(cons (member catch if multiple-value-call multiple-value-prog1
	         progn progv throw unwind-protect)))
(deftype operator-with-arg-and-forms ()
  '(cons (member block eval-when function go quote return-from the)))

(defun simple-map-subforms (fn form)
  (labels ((map-form (form)
	     (funcall fn form))
	   (map-forms (forms)
	     (mapcar #'map-form forms))
	   (map-body (body &optional doc)
	     (multiple-value-bind (forms decls doc) (parse-body body doc)
	       `(,@(when doc (list doc)) ,@decls ,@(map-forms forms))))
	   (map-let-bindings (bindings)
	     (mapcar
	      (lambda (b)
		(etypecase b
		  (symbol		b)
		  ((cons symbol null)	b)
		  ((cons symbol cons)	(list (first b)
					      (map-form (second b))))))
	      bindings))
	   (map-flet-bindings (bindings)
	     (mapcar
	      (lambda (b)
		(destructuring-bind (name lambda-list . body) b
		  `(,name ,(map-lambda-list lambda-list) ,@(map-body body t))))
	      bindings))
	   (map-lambda-list (lambda-list)
	     (mapcar (lambda (arg)
		       (destructuring-typecase arg
			 ((list-of t t . list) (x y . z)
			   (list* x (map-form y) z))
			 (t x
			   x)))
		     lambda-list)))
    (destructuring-typecase form
      (symbol x
	x)
      ((function-form lambda-expr) (_ (__ lambda-list &rest body))
	`#'(lambda ,(map-lambda-list lambda-list) ,@(map-body body t)))
      ((function-form (not (or function-name lambda-expr))) x
	(map-unknown-subforms fn (copy-tree x)))
      (operator-with-forms (op &rest forms)
	`(,op ,@(map-forms forms)))
      (operator-with-arg-and-forms  (op x &rest forms)
	`(,op ,x ,@(map-forms forms)))
      (symbol-macrolet-form (_ bindings &rest body)
	`(symbol-macrolet ,bindings ,@(map-body body)))
      (let/*-form (op bindings &rest body)
	`(,op ,(map-let-bindings bindings) ,@(map-body body)))
      (function-binding-form (op bindings &rest body)
	`(,op ,(map-flet-bindings bindings) ,@(map-body body)))
      (load-time-value-form (_ form &optional read-only-p)
	`(load-time-value ,(map-form form) ,read-only-p))
      (locally-form (_ &rest body)
	`(locally ,@(map-body body)))
      (setq-form (_ &rest var-and-forms)
	;;TODO: macroexpand variables.
	`(setq ,@(loop for (v f) on var-and-forms by #'cddr
		       nconc (list v (map-form f)))))
      (tagbody-form (_ &rest tags-and-forms)
	`(tagbody ,@(mapcar (lambda (x)
			      (typecase x
				((or symbol integer)	x)
				(t			(map-form x))))
			    tags-and-forms)))
      (special-form x
        (map-unknown-subforms fn (copy-tree x)))
      (lambda-form ((_ lambda-list &rest body) &rest forms)
	`((lambda ,(map-lambda-list lambda-list) ,@(map-body body t))
	  ,@(map-forms forms)))
      ((cons symbol list) (f &rest forms)
	`(,f ,@(map-forms forms)))
      (cons x
	(error "Malformed: ~A" x))
      (t x
	x))))

(defmacro %map-subforms (fn form &rest keys &key toplevel recursive variables
			 functions macros symbol-macros &environment env)
  (declare (ignore macros symbol-macros))
  (when variables
    (return-from %map-subforms
      `(let ,variables
	 (declare (ignorable ,@variables))
	 (%map-subforms ,fn ,form :variables nil ,@keys))))
  (when functions
    (return-from %map-subforms
      `(flet ,(mapcar (lambda (x) `(,x (&rest x))) functions)
	 (%map-subforms ,fn ,form :functions nil ,@keys))))
  (labels ((map-body (body &key doc variables functions)
	   (multiple-value-bind (forms decls doc) (parse-body body doc)
	     `(,@(when doc (list doc))
		 ,@decls
		 ,@(mapcar (lambda (x)
			     `(%map-subforms ,fn ,x :recursive ,recursive
			       :variables ,variables :functions ,functions))
			   forms))))
	 (map-let-subforms (op bindings body)
	   (let ((variables nil))
	     `(,op ,(mapcar (lambda (binding)
			      (etypecase binding
				(symbol
				 (push binding variables)
				 binding)
				((cons symbol null)
				 (push (first binding) variables)
				 binding)
				((cons symbol cons)
				 (prog1 `(,(first binding)
					   (%map-subforms
					    ,fn ,(second binding)
					    :recursive ,recursive
					    ,@(when (eq op 'let*)
					       `(:variables ,variables))))
				   (push (first binding) variables)))))
			    bindings)
		   ,@(map-body body :variables variables))))
	 (map-flet-subforms (bindings body)
	   `(flet ,(mapcar
		    (lambda (binding)
		      (destructuring-bind (name lambda-list . body) binding
			(map-lambda-subforms name lambda-list body)))
		    bindings)
	      ,@(map-body body :functions (mapcar #'first bindings))))
	 (map-lambda-subforms (op lambda-list body &optional doc)
	   `(,op ,(mapcar (lambda (arg)
			    (destructuring-typecase arg
			      ((list-of t t . list) (x y . z)
				`(,x
				  (%map-subforms ,fn ,y :recursive ,recursive
				   :variables ,(lambda-list-variables
						(ldiff lambda-list
						       (member arg lambda-list))))
				  ,@z))
			      (t x
				x)))
			  lambda-list)
		 ,@(map-body body :doc doc
			     :variables (lambda-list-variables lambda-list))))
	 (simple ()
	   (simple-map-subforms
	    (lambda (x) `(%map-subforms ,fn ,x :recursive ,recursive))
	    form))
	 (output (mapped-form)
	   (quote-tree (if toplevel
			   mapped-form
			   (funcall fn mapped-form env))
		       '%map-subforms)))
    (unless (or toplevel recursive)
      (return-from %map-subforms (output form)))
    ;;(print (list env form (macroexpand form env)))
    (setq form (macroexpand form env))
    (destructuring-typecase form
      (flet-form (op bindings . body)
	(declare (ignore op))
	(output (map-flet-subforms bindings body)))
      ((or function-binding-form symbol-macrolet-form) (op bindings . body)
	(declare (ignore body))
        (when (eq op 'macrolet)
	  (setq bindings
		(mapcar (lambda (b)
			  (destructuring-bind (name lambda-list . body) b
			    `(,name ,lambda-list
			      ,@(mapcar (lambda (f) (funcall fn f env))
					body))))
			bindings)))
        `(,op ,bindings ,(output (simple))))
      (let/*-form (op bindings . body)
	(output (map-let-subforms op bindings body)))
      ((function-form lambda-expr) #'(lambda lambda-list . body)
	(declare (ignore function))
        (output `#',(map-lambda-subforms lambda lambda-list body)))
      (lambda-form ((lambda lambda-list . body) . forms)
        (output `(,(map-lambda-subforms lambda lambda-list body t)
		  ,@(mapcar (lambda (x)
			      `(%map-subforms ,fn ,x :recursive ,recursive))
			    forms))))
      (t _
	(declare (ignore _))
	(output (simple))))))

(defun map-subforms (fn form &key recursive)
  (eval (funcall (map-form fn `(:recursive ,recursive))
		 form
		 :toplevel t)))

;;;

(defun map-form (fn keys)
  (lambda (form &rest more-keys)
    `(%map-subforms ,fn ,form ,@more-keys ,@keys)))

(defmacro %map-subforms (fn form &rest keys &key recursive &environment env)
  (funcall fn (if recursive
		  (simple-map-subforms (map-form fn keys)
				       (macroexpand form env))
		  form)
	   env))

(defun map-subforms (fn form &rest keys &key recursive)
  (declare (ignore recursive))
  (macroexpand-all (simple-map-subforms (map-form fn keys)
					(macroexpand form))))


;;;; Examples.

(defun body-form (forms)
  (setq forms
	(mapcan (lambda (form)
		  (typecase form
		    ((cons (eql progn))
		     (let ((x (body-form (rest form))))
		       (typecase x
			 ((cons (eql progn))	(rest x))
			 (t			(list x)))))
		    (t				(list form))))
		forms))
  (typecase forms
    (null		nil)
    ((list-of t)	(first forms))
    (t			`(progn ,@forms))))

#|
(defun remove-macrolet (form &optional e)
  (declare (ignore e))
  (handler-bind ((error (lambda (c) (return-from remove-macrolet form))))
    (destructuring-bind (op bindings &rest body) form
      (declare (ignore bindings))
      (if (member op '(macrolet symbol-macrolet))
	  (body-form body)
	  form))))

(defun macroexpand-all (form &optional env)
  (let* ((x (macroexpand form env))
	 (y (map-subforms #'macroexpand-all x :env env))
	 #+(or)
	 (_ (print (list :me-all form x y))))
    (remove-macrolet y)))
    ;;(map-subforms #'macroexpand-all (macroexpand form env)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mapping-subforms ((subform form
			     &key ((&environment env) (gensym) envp))
			    &body body)
  `(map-subforms (lambda (,subform ,env)
		   ,@(unless envp `((declare (ignore ,env))))
		   ,@body)
		 ,form))

(defmacro mapping-subforms* ((subform form &key recursive
			      ((&environment env) (gensym) envp))
			     &body body)
  ``(macro-map-subforms ,(lambda (,subform ,env)
			  ,@(unless envp `((declare (ignore ,env))))
			  ,@body)
			,,form :recursive ,,recursive :toplevel t))

(defmacro mapping-subforms** ((subform form &key recursive
			       ((&environment env) (gensym) envp))
			      &body body)
  (let ((fn (gensym)))
    `(flet ((,fn (,subform ,env)
	      ,@(unless envp `((declare (ignore ,env))))
	      ,@body))
       (macro-map-subforms ,fn ,form :recursive ,recursive :toplevel t))))

(defvar *macroexpand-all-hook*)

(defun %macroexpand-all (form symbol env &rest keys &key variables functions
			  macros symbol-macros)
  (declare (ignore macros symbol-macros))
  (when variables
    (return-from %macroexpand-all
      `(let ,variables
	 (declare (ignorable ,@variables))
	 (,symbol ,form :variables nil ,@keys))))
  (when functions
    (return-from %macroexpand-all
      `(flet ,(mapcar (lambda (x) `(,x (&rest x))) functions)
	 (,symbol ,form :functions nil ,@keys))))
  (labels ((map-body (body &key doc variables functions)
	     (multiple-value-bind (forms decls doc) (parse-body body doc)
	       `(,@(when doc (list doc))
		 ,@decls
		 ,@(mapcar (lambda (x)
			     `(,symbol ,x :variables ,variables
			                  :functions ,functions))
			   forms))))
	   (map-let (op bindings body)
	     (let ((variables nil))
	       `(,op ,(mapcar (lambda (binding)
				(etypecase binding
				  (symbol
				   (push binding variables)
				   binding)
				  ((cons symbol null)
				   (push (first binding) variables)
				   binding)
				  ((cons symbol cons)
				   (prog1 `(,(first binding)
					    (,symbol
					     ,(second binding)
					     ,@(when (eq op 'let*)
						`(:variables ,variables))))
				     (push (first binding) variables)))))
			      bindings)
		 ,@(map-body body :variables variables))))
	   (map-flet (bindings body)
	     `(flet ,(mapcar
		      (lambda (binding)
			(destructuring-bind (name lambda-list . body) binding
			  (map-lambda name lambda-list body)))
		      bindings)
	       ,@(map-body body :functions (mapcar #'first bindings))))
	   (map-lambda (op lambda-list body)
	     `(,op ,(mapcar (lambda (arg)
			      (destructuring-typecase arg
				((list-of t t . list) (x y . z)
				  `(,x
				    (,symbol ,y
				     :variables
				     ,(lambda-list-variables
				       (ldiff lambda-list
					      (member arg lambda-list))))
				    ,@z))
				(t x
				  x)))
			    lambda-list)
	       ,@(map-body body :doc t
			   :variables (lambda-list-variables lambda-list))))
	   (simple ()
	     (simple-map-subforms (lambda (x) `(,symbol ,x)) form))
	   (output (mapped-form)
	     (quote-tree mapped-form symbol)))
    (let ((*macroexpand-hook* *macroexpand-all-hook*))
      (setq form (macroexpand form env))
      #+(or)
      (loop while (typep form 'macro-form) do
	(let ((new-form (funcall *macroexpand-all-hook*
				 (macro-function (car form) env)
				 form
				 env)))
	  (if (eq form new-form)
	      (return-from %macroexpand-all (output form))
	      (setq form new-form)))))
    (destructuring-typecase form
      (flet-form (op bindings . body)
	(declare (ignore op))
	(output (map-flet bindings body)))
      ((or function-binding-form symbol-macrolet-form) (op bindings . body)
	(declare (ignore body))
        `(,op ,bindings ,(output (simple))))
      (let/*-form (op bindings . body)
	(output (map-let op bindings body)))
      ((function-form lambda-expr) #'(lambda lambda-list . body)
	(declare (ignore function))
        (output `#',(map-lambda lambda lambda-list body)))
      (lambda-form ((lambda lambda-list . body) . forms)
        (output `(,(map-lambda lambda lambda-list body)
		  ,@(mapcar (lambda (x) `(,symbol ,x)) forms))))
      (t _
	(declare (ignore _))
	(output (simple))))))

(defun macroexpand-all (form)
  (let ((symbol (gensym))
	(*macroexpand-all-hook* *macroexpand-hook*)
	(*macroexpand-hook* #'funcall))
    (eval `(macrolet ((,symbol (form &rest keys &environment env)
		       (apply #'%macroexpand-all form ',symbol env keys)))
	     (,symbol ,form)))))

(defun macroexpand-all-1 (form)
  (let ((expandp t))
    (labels ((expand (form)
	       (if expandp
		   (multiple-value-bind (e ep) (macroexpand-1 form)
		     (if ep
			 (progn (setq expandp nil) e)
			 (simple-map-subforms #'expand e)))
		   form)))
      (expand form))))
#+(or)
(defun macroexpand-all-1 (form)
  (let* ((hook *macroexpand-hook*)
	 (expandp t)
	 (*macroexpand-hook*
	  (lambda (fn form env)
	    (if expandp
		(progn (setq expandp nil) (funcall hook fn form env))
		form))))
    (macroexpand-all form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro declare-ignore (vars &body body &environment env)
  (body-form
   (mapcar
    (lambda (form)
      (map-subforms
       (lambda (subform e)
	 (declare (ignore e))
	 `(declare-ignore ,vars ,subform))
       (destructuring-typecase (macroexpand form env)
	 (variable-binding-form x
	   (setf x (copy-tree x))
	   (push `(declare (ignore ,@(intersection vars (binding-form-variables x))))
		 (form-body x))
	   x)
	 (t x
	   x))))
    body)))

(defmacro declare-ignore-1 (vars form &environment env)
  (setq form (destructuring-typecase (macroexpand form env)
	       (variable-binding-form x
		 (setf x (copy-tree x))
		 (push `(declare (ignore ,@(intersection vars
					    (binding-form-variables x))))
		       (form-body x))
		 x)
	       (t x
		 x)))
  (mapping-subforms* (subform form)
    `(declare-ignore-1 ,vars ,subform)))
(defmacro declare-ignore-1 (vars form &environment env)
  (map-subforms
   (lambda (subform e)
     (declare (ignore e))
     `(declare-ignore-1 ,vars ,subform))
   (destructuring-typecase (macroexpand form env)
     (variable-binding-form x
       (setf x (copy-tree x))
       (push `(declare (ignore ,@(intersection vars (binding-form-variables x))))
	     (form-body x))
       x)
     (t x
       x))))
(defmacro declare-ignore (vars &body body)
  (body-form (mapcar (lambda (x) `(declare-ignore-1 ,vars ,x)) body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+(or)
(defmacro do-subforms ((subform form &key result recursive
			((&environment env) nil envp))
		       &body body)
  `(progn
     ,(mapping-subforms* (,subform ,form :recursive ,recursive
			       ,@(when envp `(&environment ,env)))
	     ,@body
	     ,subform)
     ,result))
       
(defmacro do-subforms ((subform form &key result recursive
			((&environment env) (gensym) envp))
		       &body body)
  `(progn
     (map-subforms (lambda (,subform ,env)
		     ,@(unless envp `((declare (ignore ,env))))
		     ,@body
		     ,subform)
		   ,form
		   :recursive ,recursive)
     ,result))

(defun collect-forms (predicate form &optional env)
  (let ((result nil))
    (when (funcall predicate form env)
      (push form result))
    (do-subforms (subform form :recursive t :result (nreverse result)
		  &environment env)
      (when (funcall predicate subform env)
	(push subform result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun form-variables (form)
  (flet ((variablep (form env)
	   (and (symbolp form)
		(not (constantp form env)))))
    (remove-duplicates (collect-forms #'variablep form))))

(defun written-variables (form)
  (let ((vars nil))
    (do-subforms (subform form :result vars :recursive t &environment env)
      (let ((x (macroexpand subform env)))
	(when (typep x 'setq-form)
	  (loop for y in (cdr x) by #'cddr
	        do (let ((z (macroexpand y env)))
		     (when (symbolp z)
		       (pushnew z vars)))))))))

(defun form-bindings (form)
  (flet ((let-init-forms (bindings)
	   (mapcar (lambda (x) (if (consp x) (second x) nil)) bindings))
	 (let*-bindings (bindings)
	   (mapcan (lambda (x)
		     (if (consp x)
			 `((,(second x)) (,(first x)))
			 (list (list x) ())))
		   bindings)))
    (declare-ignore (_ __)
      (destructuring-typecase form
	(let-form (_ bindings &rest body)
	  (list (let-init-forms bindings) (let-variables bindings) body))
	(let*-form (_ bindings &rest body)
	  (append (let*-bindings bindings) (list body)))
	(lambda-form ((_ lambda-list &rest body) &rest forms)
	  (list forms (lambda-list-variables lambda-list) body))
	((function-form lambda-expr) (_ (__ lambda-list &rest body))
	  (list nil (lambda-list-variables lambda-list) body))
	(symbol-macrolet-form (_ bindings &rest body)
	  nil)))))

(defun free-variables (form &optional env bound)
  (let ((x (macroexpand form)))
    (typecase x
      (symbol
       (if (or (constantp x) (member x bound))
	   nil
	   (list x)))
      (variable-binding-form
       (loop for (forms variables) on (form-bindings x) by #'cddr
	     nconc (loop for f in forms nconc (free-variables f env bound))
	     do (loop for v in variables do (push v bound))))
      (t
       (let ((free nil))
	 (do-subforms (f x :result free &environment env)
	   (setq free (nconc (free-variables f env bound) free))))))))

(defmacro %free-variables (form &optional bound &environment env)
  (let ((x (macroexpand form env)))
    (typecase x
      (symbol
       (if (or (constantp x) (member x bound))
	   nil
	   `(list ',x)))
      (variable-binding-form
       `(nconc ,@
	 (loop for (forms variables) on (form-bindings x) by #'cddr
	       nconc (loop for f in forms collect `(%free-variables ,f ,bound))
	       do (loop for v in variables do (push v bound)))))
      (t
       (let ((free nil))
	 (mapping-subforms (f x)
	   (push `(%free-variables ,f ,bound) free))
	 `(nconc ,@free))))))
(defun free-variables (form)
  (eval `(%free-variables ,form)))

(defvar *closure-hash* (make-hash-table :test #'eq))

(defmacro plambda (lambda-list &body body)
  (with-gensyms (fn var val valp)
    `(let ((,fn (lambda ,lambda-list ,@body)))
       (setf (gethash ,fn *closure-hash*)
	     (lambda (,var &optional (,val nil ,valp))
	       (ecase ,var
		 ,@(mapcar (lambda (x) `((,x) (setf ,x (if ,valp ,val ,x))))
			   (free-variables `(lambda ,lambda-list ,@body))))))
       ,fn)))

(defun closure-variable (fn var)
  (funcall (gethash fn *closure-hash*) var))

(defun (setf closure-variable) (val fn var)
  (funcall (gethash fn *closure-hash*) var val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun form-constants (form)
  (remove-duplicates (collect-forms #'constantp form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro step-subforms (form)
  `(progn
     (format t "~&Subform: ~A" ',form)
     (read-line)
     (macro-map-subforms ,(lambda (subform env)
			    (declare (ignore env))
			    `(step-subforms ,subform))
                         ,form)))
(defmacro step-subforms (form)
  `(progn
     (format t "~&Subform: ~A" ',form)
     (read-line)
     ,(map-subforms (lambda (subform env)
		      (declare (ignore env))
		      `(step-subforms ,subform))
		    form)))
(defmacro step-subforms (form)
  `(progn
     (format t "~&Subform: ~A" ',form)
     (read-line)
     ,(mapping-subforms (subform form &environment env)
	(declare (ignore env))
        `(step-subforms ,subform))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-define (&body body)
  (let ((forms nil))
    (loop for form in (reverse body) do
	 (destructuring-typecase form
	   ((list-of (eql define) symbol t) (_ var val)
	     (declare (ignore _))
	     (setq forms `((let ((,var ,val)) ,@(or forms (list var))))))
	   (t x
	     (push x forms))))
    (body-form forms)))

(defmacro with-define (&body body)
  (let ((ignore nil)
	(result nil))
    `(let* ,(mapcan (lambda (form)
		      (setq result (gensym))
		      (destructuring-typecase form
			((list-of (eql define) symbol t) (_ var val)
			  (declare (ignore _))
			  `((,result ,val) (,var ,result)))
			(t x
			  (push result ignore)
			  `((,result ,x)))))
		    body)
       (declare (ignore ,@(delete result ignore)))
       ,result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun global-special-p (symbol)
  (eval `(flet ((foo () ,symbol))
	   (let ((,symbol ',(list nil)))
	     (and (boundp ',symbol) (eq ,symbol (foo)))))))

(defun transform-to-cps (form k &optional env)
  (destructuring-typecase (macroexpand form env)
    (atom x
     `(,k ,x))
    ((cons (eql call/cc)) (_ fn)
     (let ((cc (gensym)))
       `(let ((,cc #',k)) (funcall ,cc (funcall ,fn ,cc)))))
    ((cons (eql if)) (_ x y &optional z)
     (let ((result (gensym)))
       (transform-to-cps
	x
	`(lambda (,result)
	   (if ,result
	       ,(transform-to-cps y k)
	       ,(transform-to-cps z k))))))
    ((list-of (member let let*) null . list) (_ __ . forms)
     (transform-to-cps `(progn ,@forms) k))
    ((cons (eql let)) (_ ((var val) . bindings) . forms)
     (let ((result (gensym)))
       (transform-to-cps
	val
	`(lambda (,result) ,(transform-to-cps
			     `(let ,bindings (let ((,var ,result) ,@forms)))
			     `(lambda (,var) (,k ,var)))))))
    ((cons (eql let*)) (_ ((var val) . bindings) . forms)
     (transform-to-cps
      val
      `(lambda (,var) ,(transform-to-cps `(let* ,bindings ,@forms) k))))
    ((cons (eql progn)) (_ . forms)
     (let* ((results (loop for x in forms collect (gensym)))
	    (k `(lambda ,(last results) (,k ,(car (last results))))))
       (loop for form in (reverse (rest forms))
	     and result in (rest (reverse results)) do
	    (setq k `(lambda (,result) ,(transform-to-cps form k))))
       (transform-to-cps (first forms) k)))
    ((function-form symbol) (_ fn)
     `(,k #',fn))
    ((function-form lambda-expr) (_ (__ lambda-list &rest body))
     (let* ((result (gensym)))
       `(,k (lambda ,lambda-list
	      ,(transform-to-cps `(progn ,@body)
				 `(lambda (,result) ,result))))))
    ((cons t null) (fn)
     `(,k (,fn)))
    (t (fn . args)
     (let* ((results (loop for x in args collect (gensym)))
	    (k `(lambda ,(last results) (,k (,fn ,@results)))))
       (loop for arg in (reverse (rest args))
	     and result in (rest (reverse results)) do
	    (setq k `(lambda (,result) ,(transform-to-cps arg k))))
       (transform-to-cps (first args) k)))))

(defmacro named-let (name bindings &body body)
  (multiple-value-bind (forms decls) body
    (with-gensyms (fn start)
      `(labels ((,fn ,(let-variables bindings)
		  ,@decls
		  (tagbody
		     ,start
		     (macrolet ((,name (&rest vals &environment e)
				  (if (macroexpand '%tail-call e)
				      `(progn
					 (psetq ,@(mapcan (lambda (var val)
							    `(,var ,val))
							  ',(let-variables bindings)
							  vals))
					 (go ,',start))
				      `(funcall #',',fn ,@vals))))
		       (return-from ,fn (symbol-macrolet ((%tail-call nil))
					    ,@forms))))))
	 (,fn ,@(mapcar (lambda (val) (if (symbolp val) nil (second val)))
			bindings))))))

(defmacro defexpr (name lambda-list &body body)
  (let ((args (gensym)))
    `(defmacro ,name (&rest ,args)
       `(apply ,(lambda ,lambda-list ,@body) ',,args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun capture-bindings (vars)
  (lambda (form env)
    (declare (ignore env))
    (typecase form
      (function-form
        (let ((gs (loop for v in vars collect (gensym)))
	      (args (gensym)))
	  `(let ,(loop for v in vars and g in gs collect `(,g ,v))
	     (lambda (&rest ,args)
	       (let ,(loop for v in vars and g in gs collect `(,v ,g))
		 (apply ,form ,args))))))
      (t
        form))))

;;; "A dynamic environment contains, among other things: exit points
;;; established by unwind-protect, and bindings of dynamic variables,
;;; exit points established by catch, condition handlers, and
;;; restarts."
;;; See also CLHS 3.1.1.2.
(defmacro with-captured-special-bindings (vars &body body)
  (map-subforms (capture-bindings vars) `(progn ,@body) :recursive t))


;;;; Tests

(defvar *tests* (make-hash-table))

(defmacro deftest (name () &body body)
  `(progn
     ,(if body
	  `(setf (gethash ',name *tests*) (lambda () ,@body))
	  `(remhash ',name *tests*))
     ',name))

(defun run-tests (&key tests verbose)
  (let ((failures 0)
	(successes 0))
    (flet ((run (name fn)
	     (cond ((ignore-errors (funcall fn))
		    (when verbose
		      (format t "~&~60A ok~%" name))
		    (incf successes))
		   (t
		    (format t "~&~60A FAIL~%" name)
		    (incf failures)))
	     #+(or)
	     (format t "~&~60A ~:[FAIL~;ok~]~%"
		     name (ignore-errors (funcall fn)))))
      (etypecase tests
	(null	(maphash #'run *tests*))
	(symbol	(run tests (gethash tests *tests*)))
	(cons	(dolist (name tests)
		  (run-tests :tests name)))))
    (format t "~&Successes: ~D, failures: ~D~%" successes failures)))

(defmacro define-map-subforms-test (name &rest stuff)
  (flet ((get-key (key)
	   (let* ((x (rest (member key stuff)))
		  (y (member-if (lambda (x) (member x '(:body :input :output)))
				x)))
	     (ldiff x y))))
    (let ((body (get-key :body))
	  (input (get-key :input))
	  (output (get-key :output)))
      `(deftest ,(intern (format nil "~A-~A" 'map-subforms name)) ()
	(tree-equal
	 (map-subforms (lambda (x e) (declare (ignorable x e)) ,@body)
	               ',(first input) ,@(rest input))
	 ',(first output)
	 :test #'equal)))))

(define-map-subforms-test trivial-1
  :body 42
  :input (f x)
  :output (f 42))

(define-map-subforms-test trivial-2
  :body (1+ x)
  :input (f 42)
  :output (f 43))

(define-map-subforms-test trivial-3
  :body `(g ,x)
  :input (f x)
  :output (f (g x)))

(define-map-subforms-test trivial-4
  :body (second x)
  :input (f (g x))
  :output (f x))

(define-map-subforms-test recursively ()
  :body `(1+ ,x)
  :input (prog (a (b (c d)))
	  e
	  (f)
	  (g (h i)))
         :recursive t
  :output (block nil
	    (1+ (let (a (b (1+ (c (1+ d)))))
		  (1+ (tagbody
		       e
			 (1+ (f))
			 (1+ (g (1+ (h (1+ i)))))))))))

(define-map-subforms-test not-recursively
  :body `(1+ ,x)
  :input (a b (c d) (e (f g)))
  :output (a (1+ b) (1+ (c d)) (1+ (e (f g)))))

(define-map-subforms-test macrolet
  :body (if (numberp x) (1+ x) x)
  :input (macrolet ((a (b c) `(d ,c ,b)))
	   (a 10 20))
         :recursive t
  :output (macrolet ((a (b c) `(d ,c ,b)))
	    (d 21 11)))

(define-map-subforms-test flet-and-macrolet
  :body (if (numberp x) (1+ x) x)
  :input (flet ((f () 41))
	   (declare (optimize (safety 3)))
	   (macrolet ((f () 51))
	     (f)))
         :recursive t
  :output (flet ((f () 42))
	    (declare (optimize (safety 3)))
	    (macrolet ((f () 52))
	      53)))

(define-map-subforms-test macrolet-and-flet
  :body (if (numberp x) (1+ x) x)
  :input (macrolet ((f () 51))
	   (flet ((f () 41)
		  (g () (f)))
	     (f)))
	 :recursive t
  :output (macrolet ((f () 52))
	    (flet ((f () 42)
		   (g () 53))
	      (f))))

(define-map-subforms-test symbol-macrolet-and-flet
  :body x
  :input (symbol-macrolet ((a 42) (c 100))
	   (flet ((f (a &optional (b c c) &aux (d c)) a))
	     (f)))
	 :recursive t
  :output (symbol-macrolet ((a 42) (c 100))
	    (flet ((f (a &optional (b 100 c) &aux (d c)) a))
		(f))))

(define-map-subforms-test labels-and-macrolet
  :body (if (numberp x) (1+ x) x)
  :input (labels ((f () 41))
	   (declare (optimize (safety 3)))
	   (macrolet ((f () 51))
	     (f)))
         :recursive t
  :output (labels ((f () 42))
	    (declare (optimize (safety 3)))
	    (macrolet ((f () 52))
	      53)))

(define-map-subforms-test macrolet-and-labels
  :body (if (numberp x) (1+ x) x)
  :input (macrolet ((f () 51))
	   (labels ((f () (g))
		    (g () 41))
	     (f)))
	 :recursive t
  :output (macrolet ((f () 52))
	    (labels ((f () (g))
		     (g () 42))
	      (f))))

(define-map-subforms-test let-and-symbol-macrolet
  :body x
  :input (let ((x 42))
	   (declare (fixnum x))
	   (symbol-macrolet ((x 100))
	     x))
	 :recursive t
  :output (let ((x 42))
	    (declare (fixnum x))
	    (symbol-macrolet ((x 100))
	      100)))

(define-map-subforms-test symbol-macrolet-and-let
  :body x
  :input (symbol-macrolet ((x 100))
	   (let ((x 42))
	     x)
	   (let ((x x))
	     x))
	 :recursive t
  :output (symbol-macrolet ((x 100))
	    (let ((x 42))
	      x)
	    (let ((x 100))
	      x)))

(define-map-subforms-test symbol-macrolet-and-let*
  :body x
  :input (symbol-macrolet ((c 1))
	   (let* ((a 2) (b c) (c 3) (d c))
	     (list b d)))
         :recursive t
  :output (symbol-macrolet ((c 1))
	    (let* ((a 2) (b 1) (c 3) (d c))
	      (list b d))))

(define-map-subforms-test function-lambda
  :body (1+ x)
  :input #'(lambda (a &optional (b 1) &key (c 2) &aux (d 3))
	     (declare (fixnum a))
	     4)
  :output #'(lambda (a &optional (b 2) &key (c 3) &aux (d 4))
	      (declare (fixnum a))
	      5))

(define-map-subforms-test symbol-macrolet-and-lambda
  :body (if (stringp x) (string-upcase x) x)
  :input (symbol-macrolet ((c 1))
	   ((lambda (a &optional (b c) (c 3) (d c))
	      "doc"
	      (list b d))
	    2))
         :recursive t
  :output (symbol-macrolet ((c 1))
	    ((lambda (a &optional (b 1) (c 3) (d c))
	       "doc"
	       (list b d))
	     2)))

#+sbcl
(define-map-subforms-test sbcl-truly-the
  :body (1+ x)
  :input (sb-ext:truly-the fixnum 42)
  :output #.(if (string>= (lisp-implementation-version) "1.0.49")
		'(the fixnum 43)
		'(sb-ext:truly-the fixnum 43)))

#+sbcl
(define-map-subforms-test sbcl-named-lambda
  :body (1+ x)
  :input #'(sb-int:named-lambda foo () 42)
  :output #'(sb-int:named-lambda foo () 43))
#+(or)
(define-map-subforms-test sbcl-named-lambda
  :body (if (numberp x) (1+ x) x)
  :input (symbol-macrolet ((x 42))
	   (sb-int:named-lambda foo (x) (+ x 1)))
         :recursive t
  :output (symbol-macrolet ((x 42))
	    (sb-int:named-lambda foo (x) (+ x 2))))

;;; #+sbcl sb-cltl2:compiler-let sb-c::global-function sb-c::%funcall
;;; sb-c::%within-cleanup sb-c::%escape-fun sb-c::%%allocate-closures
;;; sb-c::%cleanup-fun sb-sys:%primitive

#+clisp
(define-map-subforms-test clisp-setf-call
  :body `(cons ,x nil)
  :input ((setf cdr) 1 x)
  :output ((setf cdr) (cons 1 nil) (cons x nil)))

;;; #+ccl ccl:nfunction ccl:compiler-let ccl::ppc-lap-function
;;; ccl::with-variable-c-frame ccl::with-c-frame ccl::fbind

;;; #+ecl ext:compiler-let

;;; #+lispworks: nothing

;;; TODO: abcl, allegro, cmu, gcl, scl, (open)mcl, cormanlisp, mkcl


;;; Local variables:
;;; eval: (put 'destructuring-typecase 'common-lisp-indent-function
;;;            '(4 &rest (&whole 2 4 . #1=(2 . #1#))))
;;; End:

;;;;  http://glennpendlay.wordpress.com/2012/02/17/got-tendonitis/
