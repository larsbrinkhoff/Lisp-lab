(defpackage :se.brinkhoff.collect-1.0
  (:nicknames :se.brinkhoff.collect :collect-1.0 :collect)
  (:use :cl)
  (:export :with-collector :with-collectors :collect
	   :define-collector :collector-bind))

#|
Design criteria:

- One or several collections at a time.
- One collection => result is collection.
- Several collections => result is last form.
- Collections of different types (list, vector, string, etc).
- Run-time resolution of type, but optimize when possible.
- Lexical binding available for the collection.
- Support keyword arguments for collection, e.g. :initial-value.
- Support keyword arguments for collect, e.g. :key (for hash tables).
|#

(in-package :se.brinkhoff.collect-1.0)

(defvar *collectors* nil)

(define-symbol-macro %collector-alist nil)

(defun ensure-first-list (list)
  (if (listp (first list))
      list
      (cons (list (first list)) (rest list))))

(defun quotedp (form)
  (typep form '(cons (eql quote) (cons * null))))

(defun element-type (typespec)
  (etypecase typespec
    (symbol
     (ecase typespec
       ((null cons list)	t)
       ((string simple-string)	'character)
       ((base-string simple-base-string)
				'base-char)
       ((vector simple-vector array simple-array)
				t)
       ((bit-vector simple-bit-vector)
				'bit)))
    (cons
     (ecase (first typespec)
       ((cons simple-vector)	t)
       ((vector array simple-array)
				(if (null (rest typespec))
				    t
				    (second typespec)))
       ((string simple-string)	'character)
       ((base-string simple-base-string)
				'base-char)
       ((bit-vector simple-bit-vector)
				'bit)))))

(defmacro define-collector (type lambda-list &body body)

  "DEFINE-COLLECTOR TYPE (RESULT BODY &KEY KEY*) DECLARATION* FORM* => TYPE

  TYPE -- a symbol naming the type handled by the collector.
  RESULT -- a parameter for the result variable.
  BODY -- a parameter for the WITH-COLLECTOR body forms.
  KEY* -- keyword parameters.
  DECLARATION* -- declarations.
  FORM* -- an implicit progn.

  Define a collector for subtypes of TYPE.  The value of FORM* will be
  the expansion of a WITH-COLLECTOR call with the :TYPE argument being a
  subtype of TYPE.  The expansion must use COLLECTOR-BIND to establish a
  handler around the forms passed in BODY for COLLECT, and lexically
  bind the symbol in RESULT to the result of the collection."

  `(eval-when (:execute :load-toplevel)
     (setf (get ',type '%collector) (lambda ,lambda-list ,@body))
     (pushnew ',type *collectors*)
     ',type))

(defmacro collector-bind ((name fn) &body body &environment env)

  "COLLECTOR-BIND (NAME FUNCTION) FORM* => RESULT

  NAME -- a symbol.
  FUNCTION -- a function.
  FORM* -- an implicit progn.
  RESULT -- the value(s) of FORM*.

  Establishes a lexical context around the body.  In this context,
  calls to COLLECT with an :INTO NAME argument will expand to the result
  of applying FUNCTION to all arguments passed to COLLECT."

  `(symbol-macrolet ((%collector-alist
		      ((,name ,fn) ,@(macroexpand-1 '%collector-alist env))))
     ,@body))

(define-collector list (result body &key name type (initial-value nil))
  (declare (ignore type))
  (let ((list (gensym))
	(tail (gensym)))
    #|
    (with-collect-expander (name (form &key into)
                            (declare (ignore into))
			    `(setf ,tail (setf (rest ,tail) (list ,form))))
    |#
    `(let* ((,list (cons nil ,initial-value))
	    (,tail (last ,list)))
       (declare (type cons ,list ,tail))
       (collector-bind (,name
       	 		,(lambda (form &key into)
       	 		   (declare (ignore into))
			   `(setf ,tail (setf (rest ,tail) (list ,form)))))
	 (symbol-macrolet ((,result (rest ,list)))
	   ,@body)))))

(define-collector vector (result body &key name type initial-value)
  (let ((vector (gensym))
	(element-type `(element-type ,type))
	(declaration nil))
    (when (quotedp type)
      (ignore-errors
	(setq element-type `(quote ,(element-type (second type)))
	      declaration
	      `((declare (type (array ,(element-type (second type)) (*))
			       ,vector))))))
    (unless initial-value
      (setq initial-value `(make-array 10 :adjustable t :fill-pointer 0
			                  :element-type ,element-type)))
    `(let ((,vector ,initial-value))
       ,@declaration
       (collector-bind (,name ,(lambda (form &key into)
				 (declare (ignore into))
				 `(vector-push-extend ,form ,vector)))
	 (symbol-macrolet ,(when result `((,result (coerce ,vector ,type))))
	   ,@body)))))

(define-collector real (result body &key name type (radix 10)
			(initial-value 0))
  (let ((gradix (gensym))
	(declared-type 'real))
    (when (quotedp type)
      (setq declared-type (second type)))
    `(let ((,gradix ,radix)
	   (,result ,initial-value))
       (declare (type ,declared-type ,result ,gradix))
       (collector-bind (,name
			,(lambda (form &key into)
			  (declare (ignore into))
			  `(setf ,result
			         (the ,declared-type
			           (+ (the ,declared-type (* ,result ,gradix))
				      (the ,declared-type ,form))))))
	 ,@body))))

(define-collector hash-table (result body &key name type (test #'eql)
			      (initial-value `(make-hash-table :test ,test)))
  (declare (ignore type))
  `(let ((,result ,initial-value))
     (declare (type hash-table ,result))
     (collector-bind (,name ,(lambda (form &key into key)
			       (declare (ignore into))
			       `(setf (gethash ,key ,result) ,form)))
       ,@body)))

(defun expand-collector (result body
			 &rest args
			 &key
			 (initial-value nil initvalp)
			 (type (if initvalp `(type-of ,(gensym)) ''list))
			 &allow-other-keys)
  (when (quotedp type)
    (dolist (supertype *collectors*)
      (when (subtypep (second type) supertype)
	(return-from expand-collector
	  (apply (get supertype '%collector) result body args))))
    (warn "Forced to do run-time resolution of collector type ~S" type))
  (let ((gtype (gensym)))
    (setf (getf args :type) gtype)
    `(let* (,@(when initvalp
	        (prog1 `((,(second type) ,initial-value))
		  (setf (getf args :initial-value) (second type))))
	    (,gtype ,type))
       (cond
	 ,@(mapcar
	     (lambda (supertype)
	       `((subtypep ,gtype ',supertype)
		 ,(apply (get supertype '%collector) result body args)))
	     *collectors*)
	 (t (error "Don't know how to collect type ~S" ,gtype)))))))

(defmacro with-collector ((&rest keys &key name &allow-other-keys) &body body)

  "WITH-COLLECTOR (&KEY NAME TYPE INITIAL-VALUE &ALLOW-OTHER-KEYS) FORM*
   => RESULT

  NAME -- a symbol naming the collector; not evaluated.  The default is NIL.
  TYPE -- the type of the collection; evalutated.  The default is 'LIST.
  INITIAL-VALUE -- initial value for the collection; evalutated.
  FORM* -- an implicit progn.
  RESULT -- the resulting collection.

  In the body, (COLLECT <object> :INTO <name>) is defined to collect
  an object into the corresponding named collection.  The default for
  NAME is NIL, which is also the default for INTO.  If NAME is not NIL,
  a variable by that name is bound to the result, and may be referenced
  anywhere within the body."

  (let ((result (or name (gensym))))
    (apply #'expand-collector result `(,@body ,result) keys)))

(defmacro with-collectors (collectors &body body)

  "WITH-COLLECTORS ((NAME &KEY TYPE INITIAL-VALUE &ALLOW-OTHER-KEYS)*) FORM*
   => RESULT

  NAME -- a symbol naming the collector; not evaluated.
  TYPE -- the type of the collection; evalutated.  The default is 'LIST.
  INITIAL-VALUE -- initial value for the collection; evalutated.
  FORM* -- an implicit progn.
  RESULT -- the value(s) of FORM*.

  In the body, (COLLECT <object> :INTO <name>) is defined to collect
  an object into the corresponding named collection.  The variable NAME
  is bound to the result, and may be referenced anywhere within the
  body."

  (etypecase collectors
    (null `(progn ,@body))
    (cons (destructuring-bind ((name &rest keys) &rest more-collectors)
	                      (ensure-first-list collectors)
	    (apply #'expand-collector
		   name
		   `((with-collectors ,more-collectors ,@body))
		   :name name
		   keys)))))

(defmacro collect (form &rest keys &key into
		   &allow-other-keys &environment env)

  "COLLECT FORM &KEY INTO &ALLOW-OTHER-KEYS => UNDEFINED

  FORM -- a form; evalutated.
  INTO -- a collection name; not evaluated.

  Store the value of FORM into the named collection.  The default name
  is NIL, which is also the default name for WITH-COLLECTOR."

  (let ((alist (macroexpand-1 '%collector-alist env)))
    (assert (listp alist) () "Failed to expand collector a-list")
    (let ((info (assoc into alist)))
      (assert (consp info) () "No collector named ~S" into)
      (apply (second info) form keys))))
