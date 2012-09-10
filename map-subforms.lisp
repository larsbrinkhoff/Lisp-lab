(in-package :cl-user)

;;; Operators using environment objects:
;;; defmacro, macroexpand, macroexpand-1, macro-function, *macroexpand-hook*,
;;; macrolet, define-compiler-macro, compiler-macro-function,
;;; define-setf-expander, defsetf, get-setf-expansion,
;;; constantp, make-load-form, make-load-form-saving-slots,
;;; deftype, typep, subtypep, upgraded-array-element-type,
;;; upgraded-complex-part-type
;;; NEW: find-class

(defun compilation-error (form)
  (handler-bind ((warning #'muffle-warning)
		 (error (lambda (c) (return-from compilation-error c))))
    (let ((*standard-output* (make-broadcast-stream))
	  (*error-output* (make-broadcast-stream)))
      (nth-value 2 (compile nil `(lambda () ,form))))))

(defvar *toplevel-form*)

(defvar *env* (make-hash-table :test #'eq))

(defun map-subforms (fn form &key recursive env)
  #+(or)
  (print (list :map-subforms form))

  (when env
    (setf (car (gethash env *env*)) form))

  (when (atom form)
    (return-from map-subforms form))
  (multiple-value-bind (form *toplevel-form*)
      (if (boundp '*toplevel-form*)
	  (values form *toplevel-form*)
	  (let ((x (copy-tree form))) (values x x)))
    (labels ((walk (subforms)
	       (unless (null subforms)
		 (let* ((sym (gensym))
			(sym1 (gensym))
			(x (first subforms))
			(unexpanded t)
			(result x)
			(fn (lambda (x e)
			      (setf (gethash e *env*) subforms)
			      (setq unexpanded nil
				    result (funcall fn x e))))
			(compile-form
			 `(macrolet
			      ((,sym (x &environment e)
				 (let ()#+(or)
				      ((*standard-output* ,*standard-output*)
				       (*error-output* ,*error-output*))
				   (funcall ,fn x e))
				 x))
			    ,*toplevel-form*)))
		   (if (symbolp x)
		       (setf (first subforms) sym1
			     compile-form
			     `(symbol-macrolet ((,sym1 (,sym ,x)))
				,compile-form))
		       (setf (first subforms) `(,sym ,x)))
		   (when (compilation-error compile-form)
		     (setq result x
			   unexpanded t))
		   ;(print (list :result result unexpanded))
		   (setf (first subforms) result)
		   (when (and (or unexpanded recursive) (consp result))
		     (walk result))
		   (walk (rest subforms))))))
      (walk (rest form))
      form)))

(defun map-subforms (fn form &key recursive env)
  (declare (ignore env))
  (when (atom form)
    (return-from map-subforms form))
  (multiple-value-bind (form *toplevel-form*)
      (if (boundp '*toplevel-form*)
	  (values form *toplevel-form*)
	  (let ((x (copy-tree form))) (values x x)))
    (labels ((walk (subforms)
	       (unless (null subforms)
		 (let* ((sym (gensym))
			(sym1 (gensym))
			(x (first subforms))
			(result #1='#:unexpanded)
			(y (if recursive (map-subforms fn x) x))
			(fn (lambda (x e) (setq result (funcall fn x e))))
			(compile-form
			 `(macrolet
			      ((,sym (x &environment e)
				 (let ((*standard-output* ,*standard-output*)
				       (*error-output* ,*error-output*))
				   (funcall ,fn x e))
				 x))
			    ,*toplevel-form*)))
		   (if (symbolp x)
		       (setf (first subforms) sym1
			     compile-form
			     `(symbol-macrolet ((,sym1 (,sym ,y)))
				,compile-form))
		       (setf (first subforms) `(,sym ,y)))
		   (when (prog1 (or (compilation-error compile-form)
				    (eq result #1#))
			   (setf (first subforms) x))
		     (setq result (if (consp x) (walk x) x)))
		   (cons result (walk (rest subforms)))))))
      (cons (first form) (walk (rest form))))))

(deftype list-of (&rest types)
  (typecase types
    (null		'null)
    (cons		`(cons ,(car types) (list-of ,@(cdr types))))
    (atom		types)))

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

(defmacro do-subforms ((subform form &key result recursive
			((&environment env) (gensym) envp))
		       &body body)
  `(progn
     (map-subforms (lambda (,subform ,env)
		     ,@(unless envp `((declare ignore ,env)))
		     ,@body
		     ,subform)
		   ,form
		   :recursive ,recursive)
     ,result))

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

(deftype let-form () '(list-of (eql let) list . list))
(deftype let*-form () '(list-of (eql let*) list . list))
(deftype setq-form () '(list-of (eql setq) . list)
(deftype lambda-expr () '(list-of (eql lambda) list . list))
(deftype lambda-form () '(list-of lambda-expr . list))
(deftype function-name () '(or symbol (list-of (eql setf) symbol)))
(deftype function-form (&optional (arg '(or function-name lambda-expr)))
  `(list-of (eql function) ,arg))
(deftype special-form ()
  '(cons (and symbol (satisfies special-operator-p))
              list))
(deftype macro-form ()
  '(cons (and symbol (satisfies macro-function))
         t)) ;Allow dotted lists.
(deftype symbol-macrolet-form () '(list-of (eql symbol-macrolet) list . list))

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
  (destructuring-typecase form
    ((variable-binding-form let let*) (_ bindings &rest body)
      (declare (ignore _ body))
      (let-variables bindings))
    (lambda-form ((_ lambda-list &rest body) &rest forms)
      (declare (ignore _ body forms))
      (lambda-list-variables lambda-list))
    ((function-form lambda-expr) (_ (__ lambda-list &rest body))
      (declare (ignore _ __ body))
      (lambda-list-variables lambda-list))
    (t x
      (error "Not a variable binding form: ~S" x))))

(defun binding-form-functions (form)
  (destructuring-bind (op bindings . body) form
    (declare (ignore op form))
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

(defmacro mapping-subforms ((subform form &optional (env (gensym) envp))
			    &body body)
  `(map-subforms (lambda (,subform ,env)
		   ,@(unless envp `((declare (ignore ,env))))
		   ,@body)
		 ,form))
(defun quote-tree (x &optional (unquote (list)))
  (cond
    ((atom x)
     `(quote ,x))
    ((eq (car x) unquote)
     x)
    (t		
     `(cons ,(quote-tree (car x) unquote) ,(quote-tree (cdr x) unquote)))))
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

(defmacro %macroexpand-all (form &environment env)
  (destructuring-typecase (macroexpand form env)
    ((cons (member macrolet symbol-macrolet)) (op bindings . body)
      ;; Macro bindings remain in the expansion, but dissapear in the result.
      `(,op ,bindings
        ,@(rest (mapping-subforms (subform `(locally ,@body))
		  `(%macroexpand-all ,subform)))))
    ((cons (member let let* flet labels)) (&whole x op bindings . _)
      (declare (ignore _))
      ;; Lexical bindings remain in the expansion, and also in the result.
      (if (member op '(flet labels))
	  (setq bindings (mapcar (lambda (b) `(,(first b) ,(second b) nil))
				 bindings))
	  (setq bindings (mapcar (lambda (b) (if (symbolp b) b (car b)))
				 bindings)))
      `(,op ,bindings ,(quote-tree (mapping-subforms (subform x)
				     `(%macroexpand-all ,subform))
				   '%macroexpand-all)))
    (lambda-form (&whole x (op lambda-list . body) . forms)
      (declare (ignore op body forms))
      `(let ,(lambda-list-variables lambda-list)
	 ,(quote-tree (mapping-subforms (subform x)
			 `(%macroexpand-all ,subform))
		       '%macroexpand-all)))
    (t x
      ;; Everything else just goes into the result.
      (quote-tree (mapping-subforms (subform x)
		    `(%macroexpand-all ,subform))
		  '%macroexpand-all))))
(defun macroexpand-all (form)
  (eval `(%macroexpand-all ,form)))

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

(deftype declaration-expr () '(cons (eql declare) list))
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

(defun map-subforms (fn form &key recursive env)
  (labels ((map-form (form)
	     (when recursive
	       (setq form (map-subforms fn form :recursive t :env env)))
	     (funcall fn form nil))
	   (map-forms (forms)
	     (loop for x in forms collect (map-form x)))
	   (map-body (body &optional doc)
	     (multiple-value-bind (forms decls doc) (parse-body body doc)
	       `(,@(when doc (list doc)) ,@decls ,@(map-forms forms)))))
    (destructuring-typecase (macroexpand form env)
      (symbol x
	x)
      ((cons (eql block)) (_ tag &rest forms)
        `(block ,tag ,@(map-forms forms)))
      ((cons (eql catch)) (_ &rest forms)
        `(catch ,@(map-forms forms)))
      ((cons (eql eval-when)) (_ situations &rest forms)
        `(eval-when ,situations ,@(map-forms forms)))
      ((cons (eql flet)) (_ bindings &rest body)
        `(flet ,bindings ,@(map-body body)))
      ((function-form lambda-expr) (_ (__ lambda-list &rest body))
        `#'(lambda ,lambda-list ,@(map-body body)))
      ((cons (eql function)) (_ name)
        `#',name)
      ((cons (eql go)) (_ tag)
        `(go ,tag))
      ((cons (eql if)) (_ form then &optional (else nil elsep))
        `(if ,(map-form form)
	     ,(map-form then)
	     ,@(when elsep `(,(map-form else)))))
      ((cons (eql labels)) (_ bindings &rest body)
        `(labels ,bindings ,@(map-body body)))
      ((cons (member let let*)) (let bindings &rest body)
        (let ((mapped-bindings
	       (mapcar
		(lambda (b)
		  (etypecase b
		    (symbol		b)
		    ((cons symbol null)	b)
		    ((cons symbol cons)	(list (first b)
					      (map-form (second b))))))
		bindings)))
	  `(,let ,mapped-bindings ,@(map-body body))))
      ((cons (eql load-time-value)) (_ form &optional read-only-p)
        `(load-time-value ,(map-form form) ,read-only-p))
      ((cons (eql locally)) (_ &rest body)
        `(locally ,@(map-body body)))
      ((cons (eql macrolet)) (_ bindings &rest body)
        (let ((mapped-bindings
	       (mapcar
		(lambda (b)
		  (destructuring-bind (name lambda-list . body) b
		    `(,name ,lambda-list ,@(map-body body))))
		bindings)))
	  `(macrolet ,mapped-bindings ,@(map-body body))))
      ((cons (eql multiple-value-call)) (_ &rest forms)
        `(multiple-value-call ,@(map-forms forms)))
      ((cons (eql multiple-value-prog1)) (_ &rest forms)
        `(multiple-value-prog1 ,@(map-forms forms)))
      ((cons (eql progn)) (_ &rest forms)
        `(progn ,@(map-forms forms)))
      ((cons (eql progv)) (_ &rest forms)
        `(progv ,@(map-forms forms)))
      ((cons (eql quote)) (_ object)
        `',object)
      ((cons (eql return-from)) (_ tag &optional form)
        `(return-from ,tag ,(map-form form)))
      (setq-form (_ &rest var-and-forms)
        `(setq ,@(loop for (v f) on var-and-forms by #'cddr
		       nconc (list v (map-form f)))))
      ((cons (eql symbol-macrolet)) (_ bindings &rest body)
        `(symbol-macrolet ,bindings ,@(map-body body)))
      ((cons (eql tagbody)) (_ &rest tags-and-forms)
        `(tagbody ,@(mapcar (lambda (x)
			      (typecase x
				((or symbol integer)	x)
				(t			(map-form x))))
			    tags-and-forms)))
      ((cons (eql the)) (_ type form)
        `(the ,type ,(map-form form)))
      ((cons (eql throw)) (_ tag form)
        `(throw ,(map-form tag) ,(map-form form)))
      ((cons (eql unwind-protect)) (_ &rest forms)
        `(unwind-protect ,@(map-forms forms)))
      (special-form x
        (error "Unknown special form: ~A" x))
      (lambda-form ((_ lambda-list &rest body) &rest forms)
        `((lambda ,lambda-list ,@(map-body body t)) ,@(map-forms forms)))
      ((cons symbol list) (f &rest forms)
        `(,f ,@(map-forms forms)))
      (cons x
	(error "Malformed: ~A" x))
      (t x
        x))))

(deftype operator-with-forms ()
  '(cons (member catch if multiple-value-call multiple-value-prog1
	         progn progv throw unwind-protect)))
(deftype operator-with-arg-and-forms ()
  '(cons (member block eval-when function go quote return-from the)))

(defmacro %map-subforms (fn form &key toplevel recursive &environment env)
  (labels ((map-form (form)
	     (if (or toplevel recursive)
		 `(%map-subforms ,fn ,form :recursive ,recursive)
		 form))
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
		  `(,name ,lambda-list ,@(map-body body))))
	      bindings))
	   (map-lambda-list (lambda-list)
	     lambda-list))
    (quote-tree
     (let ((mapped-form
      (destructuring-typecase (macroexpand (prog1 form (print (list :form form))) env)
        (symbol x
   	  x)
        (operator-with-forms (op &rest forms)
          `(,op ,@(map-forms forms)))
        (operator-with-arg-and-forms  (op x &rest forms)
          `(,op ,x ,@(map-forms forms)))
        ((variable-binding-form let let*) (op bindings &rest body)
	  `(,op ,(map-let-bindings bindings) ,@(map-body body)))
        (function-binding-form (op bindings &rest body)
	  `(,op ,(map-flet-bindings bindings) ,@(map-body body)))
        ((function-form lambda-expr) (_ (__ lambda-list &rest body))
          `#'(lambda ,(map-lambda-list lambda-list) ,@(map-body body)))
        ((cons (eql load-time-value)) (_ form &optional read-only-p)
          `(load-time-value ,(map-form form) ,read-only-p))
        ((cons (eql locally)) (_ &rest body)
          `(locally ,@(map-body body)))
        (setq-form (_ &rest var-and-forms)
	  ;;TODO: macroexpand variables.
          `(setq ,@(loop for (v f) on var-and-forms by #'cddr
		         nconc (list v (map-form f)))))
        ((cons (eql symbol-macrolet)) (_ bindings &rest body)
          `(symbol-macrolet ,bindings ,@(map-body body)))
        ((cons (eql tagbody)) (_ &rest tags-and-forms)
          `(tagbody ,@(mapcar (lambda (x)
				(typecase x
				  ((or symbol integer)	x)
				  (t			(map-form x))))
			      tags-and-forms)))
        (special-form x
          (error "Unknown special form: ~A" x))
        (lambda-form ((_ lambda-list &rest body) &rest forms)
          `((lambda ,(map-lambda-list lambda-list) ,@(map-body body t))
	    ,@(map-forms forms)))
        ((cons symbol list) (f &rest forms)
          `(,f ,@(map-forms forms)))
	(cons x
	  (error "Malformed: ~A" x))
        (t x
          x))))
       (if toplevel
	   mapped-form
	   (funcall fn mapped-form env)))
    '%map-subforms)))
(defmacro %map-subforms (fn form &key toplevel recursive &environment env)
  (labels ((map-form (form)
	     (if (or toplevel recursive)
		 `(%map-subforms ,fn ,form :recursive ,recursive)
		 form))
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
		  `(,name ,lambda-list ,@(map-body body))))
	      bindings))
	   (map-lambda-list (lambda-list)
	     lambda-list)
	   (fn (mapped-form)
	     (if toplevel
		 mapped-form
		 (funcall fn mapped-form env))))
    (destructuring-typecase (macroexpand (prog1 form (print (list :form form))) env)
      (symbol x
	`',(fn x))
      (operator-with-forms (op &rest forms)
	(quote-tree (fn `(,op ,@(map-forms forms))) '%map-subforms))
      (operator-with-arg-and-forms  (op x &rest forms)
	(quote-tree (fn `(,op ,x ,@(map-forms forms))) '%map-subforms))
      ((variable-binding-form let let*) (op bindings &rest body)
	`(,op ,bindings
	   ,(quote-tree `(fn (,op ,(map-let-bindings bindings) ,@(map-body body)))
			'%map-subforms)))
      (function-binding-form (op bindings &rest body)
	`(,op ,bindings
	   ,(quote-tree (fn `(,op ,(map-flet-bindings bindings)
			        ,@(map-body body)))
			'%map-subforms)))
      ((function-form lambda-expr) (_ (__ lambda-list &rest body))
	(quote-tree (fn `#'(lambda ,(map-lambda-list lambda-list)
			     ,@(map-body body)))
		    '%map-subforms))
      ((cons (eql load-time-value)) (_ form &optional read-only-p)
	(quote-tree (fn `(load-time-value ,(map-form form) ,read-only-p))
		    '%map-subforms))
      ((cons (eql locally)) (_ &rest body)
	(quote-tree (fn `(locally ,@(map-body body))) '%map-subforms))
      (setq-form (_ &rest var-and-forms)
	;;TODO: macroexpand variables.
	(quote-tree
	 (fn `(setq ,@(loop for (v f) on var-and-forms by #'cddr
			    nconc (list v (map-form f)))))
	 '%map-subforms))
      ((cons (eql symbol-macrolet)) (_ bindings &rest body)
	`(symbol-macrolet ,bindings
	   ,(quote-tree (fn `(symbol-macrolet ,bindings ,@(map-body body)))
			'%map-subforms)))
      ((cons (eql tagbody)) (_ &rest tags-and-forms)
	(quote-tree
	 (fn `(tagbody ,@(mapcar (lambda (x)
				   (typecase x
				     ((or symbol integer)	x)
				     (t			(map-form x))))
				 tags-and-forms)))
	 '%map-subforms))
      (special-form x
	(error "Unknown special form: ~A" x))
      (lambda-form ((_ lambda-list &rest body) &rest forms)
	(quote-tree
	 (fn `((lambda ,(map-lambda-list lambda-list) ,@(map-body body t))
	       ,@(map-forms forms)))
	 '%map-subforms))
      ((cons symbol list) (f &rest forms)
	(quote-tree (fn `(,f ,@(map-forms forms))) '%map-subforms))
      (cons x
	(error "Malformed: ~A" x))
      (t x
	`',(fn x)))))
(defun map-subforms (fn form &key recursive env)
  (eval `(%map-subforms ,fn ,form :toplevel t :recursive ,recursive)))

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
		  `(,name ,lambda-list ,@(map-body body))))
	      bindings))
	   (map-lambda-list (lambda-list)
	     lambda-list))
    (destructuring-typecase form
      (symbol x
	x)
      ((function-form lambda-expr) (_ (__ lambda-list &rest body))
	`#'(lambda ,(map-lambda-list lambda-list) ,@(map-body body)))
      (operator-with-forms (op &rest forms)
	`(,op ,@(map-forms forms)))
      (operator-with-arg-and-forms  (op x &rest forms)
	`(,op ,x ,@(map-forms forms)))
      ((variable-binding-form let let*) (op bindings &rest body)
	`(,op ,(map-let-bindings bindings) ,@(map-body body)))
      (function-binding-form (op bindings &rest body)
	`(,op ,(map-flet-bindings bindings) ,@(map-body body)))
      ((cons (eql load-time-value)) (_ form &optional read-only-p)
	`(load-time-value ,(map-form form) ,read-only-p))
      ((cons (eql locally)) (_ &rest body)
	`(locally ,@(map-body body)))
      (setq-form (_ &rest var-and-forms)
	;;TODO: macroexpand variables.
	`(setq ,@(loop for (v f) on var-and-forms by #'cddr
		       nconc (list v (map-form f)))))
      ((cons (eql symbol-macrolet)) (_ bindings &rest body)
	`(symbol-macrolet ,bindings ,@(map-body body)))
      ((cons (eql tagbody)) (_ &rest tags-and-forms)
	`(tagbody ,@(mapcar (lambda (x)
			      (typecase x
				((or symbol integer)	x)
				(t			(map-form x))))
			    tags-and-forms)))
      (special-form x
	(error "Unknown special form: ~A" x))
      (lambda-form ((_ lambda-list &rest body) &rest forms)
	`((lambda ,(map-lambda-list lambda-list) ,@(map-body body t))
	  ,@(map-forms forms)))
      ((cons symbol list) (f &rest forms)
	`(,f ,@(map-forms forms)))
      (cons x
	(error "Malformed: ~A" x))
      (t x
	x))))

(defmacro %map-subforms (fn form &key toplevel recursive &environment env)
  ;;(declare (optimize debug))
  ;;(print (list :env env))
  ;;(print (list :unexpanded form))
  ;;(break)
  (setq form (macroexpand form env))
  ;;(print (list :expanded form))
  ;;(break)
  (let* ((mapped-form
	  (simple-map-subforms
	   (lambda (x)
	     (if (or toplevel recursive)
		 `(%map-subforms ,fn ,x :recursive ,recursive)
		 x))
	   form))
	 (result
	  (quote-tree
	   (if toplevel
	       mapped-form
	       (funcall fn mapped-form env))
	   '%map-subforms)))
    (destructuring-typecase form
      (variable-binding-form form
	(let ((symbols (binding-form-variables form)))
	  (if t ;(typep form 'let-form)
	      (setf (second (third (third result)))
		    `(let ,symbols
		       (declare (ignorable ,@symbols))
		       ,(second (third (third result))))
		    result result)
	      `(let* ,symbols (declare (ignorable ,@symbols)) ,result))))
      ((or function-binding-form symbol-macrolet-form) (op bindings . body)
	(declare (ignore body))
	`(,op ,bindings ,result))
      (t _
	(declare (ignore _))
	result))))
(defun map-subforms (fn form &key recursive env)
  (declare (ignore env))
  (eval `(%map-subforms ,fn ,form :toplevel t :recursive ,recursive)))


;;;; Example usages.

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

(defun form-variables (form)
  (let ((vars nil))
    (do-subforms (subform form :result vars :recursive t &environment env)
      (let ((x (macroexpand subform env)))
	(when (symbolp x)
	  (pushnew x vars))))))

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
	  (list nil (lambda-list-variables lambda-list) body))))))

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

(defmacro step-subforms (form)
  `(progn
     (format t "~&Subform: ~A" ',form)
     (read-line)
     ,(map-subforms (lambda (subform env) `(step-subforms ,subform))
		    form)))
(defmacro step-subforms (form)
  `(progn
     (format t "~&Subform: ~A" ',form)
     (read-line)
     ,(mapping-subforms (subform form env)
        `(step-subforms ,subform))))

(defun global-special-p (symbol)
  (eval `(flet ((foo () ,symbol))
	   (let ((,symbol ',(list nil)))
	     (and (boundp ',symbol) (eq ,symbol (foo)))))))

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

(defun run-tests (&optional tests)
  (flet ((run (name fn)
	   (format t "~&~60A ~:[FAIL~;ok~]~%"
		   name (ignore-errors (funcall fn)))))
    (etypecase tests
      (null	(maphash #'run *tests*))
      (symbol	(run tests (gethash tests *tests*)))
      (cons	(dolist (name tests)
		  (run-tests name))))))

(deftest map-subforms-recursively ()
  (tree-equal
   (map-subforms (lambda (x e) (declare (ignore e)) `(1+ ,x))
		 '(prog (a (b (c d)))
		   e
		   (f)
		   (g (h i)))
		 :recursive t)
   '(block nil
     (1+ (let (a (b (1+ (c (1+ d)))))
	   (1+ (tagbody
		e
		  (1+ (f))
		  (1+ (g (1+ (h (1+ i))))))))))))

(deftest map-subforms-not-recursively ()
  (tree-equal
   (map-subforms (lambda (x e) (declare (ignore e)) `(1+ ,x))
		 '(a b (c d) (e (f g))))
   '(a (1+ b) (1+ (c d)) (1+ (e (f g))))))

(deftest map-subforms-macrolet ()
  (tree-equal
   (map-subforms (lambda (x e) (declare (ignore e)) (if (numberp x) (1+ x) x))
		 '(macrolet ((a (b c) `(d ,c ,b)))
		   (a 10 20))
		 :recursive t)
   '(macrolet ((a (b c)	`(d ,c ,b)))
      (d 21 11))))

(deftest map-subforms-flet-and-macrolet ()
  (tree-equal
   (map-subforms (lambda (x e) (declare (ignore e)) (if (numberp x) (1+ x) x))
		 '(flet ((f () 41))
		    (macrolet ((f () 51))
		      (f)))
		 :recursive t)
   '(flet ((f () 42))
     (macrolet ((f () 52))
       52))))

(deftest map-subforms-macrolet-and-flet ()
  (tree-equal
   (map-subforms (lambda (x e) (declare (ignore e)) (if (numberp x) (1+ x) x))
		 '(macrolet ((f () 51))
		    (flet ((f () 41))
		      (f)))
		 :recursive t)
   '(macrolet ((f () 52))
     (flet ((f () 42))
       (f)))))

(deftest map-subforms-let-and-symbol-macrolet ()
  (tree-equal
   (map-subforms (lambda (x e) (declare (ignore e)) x)
		 '(let ((x 42))
		    (symbol-macrolet ((x 100))
		      x))
		 :recursive t)
   '(let ((x 42))
     (symbol-macrolet ((x 100))
       100))))

(deftest map-subforms-symbol-macrolet-and-let ()
  (tree-equal
   (map-subforms (lambda (x e) (declare (ignore e)) x)
		 '(symbol-macrolet ((x 100))
		    (let ((x 42))
		      x))
		 :recursive t)
   '(symbol-macrolet ((x 100))
      (let ((x 42))
        x))))

(deftest map-subforms-symbol-macrolet-and-let-2 ()
  (tree-equal
   (map-subforms (lambda (x e) (declare (ignore e)) x)
		 '(symbol-macrolet ((x 100))
		    (let ((x x))
		      x))
		 :recursive t)
   '(symbol-macrolet ((x 100))
      (let ((x 100))
        x))))

(deftest map-subforms-symbol-macrolet-and-let* ()
  (tree-equal
   (map-subforms (lambda (x e) (declare (ignore e)) x)
		 '(symbol-macrolet ((x 100))
		    (let* ((x x) (y x))
		      x))
		 :recursive t)
   '(symbol-macrolet ((x 100))
      (let* ((x 100) (y x))
        x))))


;;; Local variables:
;;; eval: (put 'destructuring-typecase 'common-lisp-indent-function
;;;            '(4 &rest (&whole 2 4 . #1=(2 . #1#))))
;;; End:
