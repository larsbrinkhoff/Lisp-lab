#|
	TODO:
	- Pixel filters.
	- Fresnel stuff.
	- SAH.
	- HDRI environment maps.
	- Environment map importance sampling.
	- Spectral sampling.
	- Sky model.
	- Multiple importance sampling.
	- Randomized/scrambled QMC.
	- Bidirectional path tracing.
	- Metropolis light transport.
|#

(require :ltk)
(require :hdri-io)
(require :utah-teapot)
(require :colorimetric-data)
;(require :mt19937)

(defpackage :metro
  (:use :cl :ltk :se.brinkhoff.colorimetric-data))

(in-package :metro)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(deftype fast-float (&rest stuff) `(double-float ,@stuff))
(setf *read-default-float-format* 'double-float)

(deftype box (&optional (type 'fast-float))
  `(simple-array ,type ()))

(defmacro make-box (&optional (type 'fast-float))
  `(make-array nil :element-type ',type))

(deftype value (type)
  `(values ,type &optional))

(deftype index ()
  `(integer 0 ,(1- most-positive-fixnum)))

(deftype image-index ()
  `(integer 0 ,(floor most-positive-fixnum 4)))

(defconstant 2pi (coerce (* 2 pi) 'fast-float))
(defconstant 1/pi (coerce (/ 1 pi) 'fast-float))

(defvar *canvas*)
(defvar *image*)

(defun draw-line (x1 y1 x2 y2 &optional r g b)
  (let ((item (create-line* *canvas* x1 y1 x2 y2)))
    (when r
      (itemconfigure *canvas* item "fill"
		     (format nil "#~2,'0X~2,'0X~2,'0X" r g b)))))

(defun draw-arrow (x1 y1 x2 y2)
  (let ((item (create-line* *canvas* x1 y1 x2 y2)))
    (itemconfigure *canvas* item "arrow" "last")))

(let* ((b-cons (list 0))
       (g-cons (cons 0 b-cons))
       (r-cons (cons 0 g-cons))
       (color (list (list r-cons))))
  (defun draw-point (x y &optional (r 0) (g 0) (b 0))
    (setf (car r-cons) r)
    (setf (car g-cons) g)
    (setf (car b-cons) b)
    (image-setpixel *image* color x y (1+ x) (1+ y))
    nil))

(defmacro with-window ((&key (width 400) (height 300)) &body body)
  `(with-ltk ()
     (let ((*canvas* (make-instance 'canvas
				     :background "white"
				     :width ,width
				     :height ,height))
	   (*image* (make-instance 'photo-image
				   :width ,width
				   :height ,height)))
       (pack *canvas*)
       (clear *canvas*)
       (create-image *canvas* 0 0 :image *image*)
       (multiple-value-prog1 (progn ,@body)
	 (sleep .1)))))

(defun plot-function (fn x1 x2 &key (width 800) (height 600)
		      (colors '#1=((0 0 0) . #1#))
		      print-stats min max)
  (when (functionp fn)
    (setf fn (list fn)))
  (with-window (:width width :height height)
    (let* ((dx (/ (- x2 x1) width))
	   minx maxx
	   (min (loop with min = (or min 1e100) for f in fn
		      minimize (loop for x from x1 to x2 by dx
				     for y = (funcall f x)
				     when (< y min)
				     do (setq min y minx x)
				     finally (return min))))
	   (max (or max (loop with max = -1e100 for f in fn
			   maximize (loop for x from x1 to x2 by dx
				       for y = (funcall f x)
				       when (> y max)
				       do (setq max y maxx x)
				       finally (return max)))))
	   (range (* 1.1 (- max min))))
      (when print-stats
	(format t "~&min = ~F (x = ~F)~%max = ~F (x = ~F)~%" min minx max maxx))
      (incf max (* .05 range))
      (decf min (* .05 range))
      (flet ((y (x) (- height (round (* (/ height range) (- x min))))))
	(when (<= x1 0.0 x2)
	  (loop for y from 0 to (1- height)
		and w = (round (- (/ x1 dx))) do
		(when (zerop (logand (round y 20) 1))
		  (draw-line w y w (1+ y)))))
	(loop for x from 0 to (- width 2)
	      and w from x1 to x2 by dx do
	      (loop for f in fn and c in colors do
		   (apply #'draw-line
			  x (y (funcall f w))
			  (1+ x) (y (funcall f (+ w dx))) c))
	      (when (and (<= min 0.0 max)
			 (zerop (logand (round x 20) 1)))
		(let ((ymaxlog10 (expt 10 (round (log max 10)))))
		  (draw-line x (y ymaxlog10) (1+ x) (y ymaxlog10)))
		(draw-line x (y 0.0) (1+ x) (y 0.0))))))))

#+(or)
(progn
  (defstruct (vec (:constructor make-vec (x y z)))
    (x 0.0 :type fast-float)
    (y 0.0 :type fast-float)
    (z 0.0 :type fast-float))

  (defun vec (x y z)
    (make-vec (coerce x 'fast-float)
	      (coerce y 'fast-float)
	      (coerce z 'fast-float)))

  (define-compiler-macro vec (x y z)
    `(make-vec (coerce ,x 'fast-float)
               (coerce ,y 'fast-float)
               (coerce ,z 'fast-float))))

;;#+(or)
(progn
  (deftype vec () '(simple-array fast-float (3)))

  (defun vec (x y z)
    (let ((vec (make-array 3 :element-type 'fast-float)))
      (setf (aref vec 0) (coerce x 'fast-float))
      (setf (aref vec 1) (coerce y 'fast-float))
      (setf (aref vec 2) (coerce z 'fast-float))
      vec))

  (define-compiler-macro vec (x y z)
    (let ((vec (gensym)))
      (flet ((to-float (n)
	       (if (realp n)
		   (coerce n 'fast-float)
		   `(coerce ,n 'fast-float))))
	`(let ((,vec (make-array 3 :element-type 'fast-float)))
	   (setf (aref ,vec 0) ,(to-float x))
	   (setf (aref ,vec 1) ,(to-float y))
	   (setf (aref ,vec 2) ,(to-float z))
	   ,vec))))

  (defmacro vec-x (vec) `(the fast-float (aref (the vec ,vec) 0)))
  (defmacro vec-y (vec) `(the fast-float (aref (the vec ,vec) 1)))
  (defmacro vec-z (vec) `(the fast-float (aref (the vec ,vec) 2))))

(defmacro with-vec ((x y z) vec &body body)
  (let ((g (gensym)))
    `(let  ((,g (the vec ,vec)))
       (declare (type vec ,g))
       (let ((,x (vec-x ,g))
	     (,y (vec-y ,g))
	     (,z (vec-z ,g)))
	 (declare (fast-float ,x ,y ,z))
	 ,@body))))

(defstruct (ray (:constructor make-ray (pos dir nm)))
  (pos (vec 0 0 0) :type vec)
  (dir (vec 0 0 0) :type vec)
  (nm 0.0 :type fast-float)
  (weight 1.0 :type fast-float))

#+(or)
(progn
  (defclass ray ()
    ((pos :initarg :pos :accessor ray-pos)
     (dir :initarg :dir :accessor ray-dir)
     (nm :initarg :nm :accessor nm)))

  (defun make-ray (pos dir nm)
    (make-instance 'ray :pos pos :dir dir :nm nm)))

(defmethod print-object ((ray ray) stream)
  (print-unreadable-object (ray stream :type t)
    (format stream "(~5,2f ~5,2f ~5,2f) (~2,2f ~2,2f ~2,2f)"
	    (vec-x (ray-pos ray)) (vec-y (ray-pos ray)) (vec-z (ray-pos ray))
	    (vec-x (ray-dir ray)) (vec-y (ray-dir ray)) (vec-z (ray-dir ray)))))

(defun no-bump (normal point)
  (declare (ignore point))
  normal)

(defclass material ()
  ((emit :type fast-float :initarg :emit :accessor emit :initform 0.0)
   (specular :type fast-float :initarg :specular :accessor specular :initform 0.0)
   (transmission :type fast-float :initarg :transmission :accessor transmission :initform 0.0)
   (n1 :type fast-float :initarg :n1 :accessor n1 :initform 0.0)
   (n2 :type fast-float :initarg :n2 :accessor n2 :initform 0.0)
   (bump :type function :initarg :bump :accessor bump :initform #'no-bump)
   (diffuse :type fast-float :initarg :diffuse :accessor diffuse :initform 0.0)))

(defun make-material (&rest args)
  (apply #'make-instance 'material args))

(defclass shape ()
  ((name :initarg :name :accessor name :initform nil)
   (material :initarg :material :accessor material)))

(defmethod print-object :around ((shape shape) stream)
  (if (name shape)
      (prin1 (name shape) stream)
      (call-next-method)))

(defclass sphere (shape)
  ((pos    :initarg :pos :accessor pos)
   (radius :initarg :radius :accessor radius)))

(defmethod print-object ((sphere sphere) stream)
  (print-unreadable-object (sphere stream :type t)
    (format stream "pos ~a, radius ~a" (pos sphere) (radius sphere))))

(defgeneric intersect (object ray))
(defgeneric normal (object pos))

(defmethod intersect ((sphere sphere) ray)
  (with-vec (spx spy spz) (pos sphere)
    (with-vec (rpx rpy rpz) (ray-pos ray)
      (let ((r (radius sphere)))
	(declare (fast-float r))
	(let ((vx (- rpx spx))
	      (vy (- rpy spy))
	      (vz (- rpz spz)))
	  (declare (fast-float vy vy vz))
	  (with-vec (dx dy dz) (ray-dir ray)
	    (let ((a (+ (* vx dx) (* vy dy) (* vz dz)))
		  (b (+ (* vx vx) (* vy vy) (* vz vz) (- (* r r)))))
	      (declare (fast-float a b))
	      (let ((c (- (* a a) b)))
		(declare (fast-float c))
		(if (minusp c)
		    2e30
		    (let ((d (sqrt c)))
		      (declare (fast-float d))
		      (let ((e (- (+ a d))))
			(declare (fast-float e))
			(if (plusp e)
			    e
			    (let ((f (- d a)))
			      (declare (fast-float f))
			      (if (plusp f)
				  f
				  2e30))))))))))))))

(defmethod normal ((sphere sphere) pos)
  (with-vec (px py pz) pos
    (with-vec (sx sy sz) (pos sphere)
      (let ((normal (vec (- px sx) (- py sy) (- pz sz))))
	(normalize normal)))))

(defmethod bounding-box ((sphere sphere))
  (with-vec (x y z) (pos sphere)
    (let ((r (radius sphere)))
      (list
       (vec (- x r) (- y r) (- z r))
       (vec (+ x r) (+ y r) (+ z r))))))

(defclass ring (shape)
  ((pos :initarg :pos :accessor pos)
   (nor :initarg :normal :accessor nor)
   (rmin2 :initarg :rmin2 :accessor rmin2)
   (rmax2 :initarg :rmax2 :accessor rmax2)))

(defmethod intersect ((ring ring) ray)
  (with-vec (nx ny nz) (nor ring)
    (with-vec (rx ry rz) (pos ring)
      (with-vec (px py pz) (ray-pos ray)
	(with-vec (dx dy dz) (ray-dir ray)
	  (let ((a (+ (* dx nx) (* dy ny) (* dz nz))))
	    (declare (fast-float a))
	    (if (zerop a)
		2e30
		(let* ((vx (- rx px))
		       (vy (- ry py))
		       (vz (- rz pz))
		       (t1 (/ (+ (* vx nx) (* vy ny) (* vz nz)) a)))
		  (if (minusp t1)
		      2e30
		      (let* ((qx (- (* t1 dx) vx))
			     (qy (- (* t1 dy) vy))
			     (qz (- (* t1 dz) vz))
			     (q2 (+ (* qx qx) (* qy qy) (* qz qz))))
			(if (<= (the fast-float (rmin2 ring))
				q2
				(the fast-float (rmax2 ring)))
			    t1
			    2e30)))))))))))

(defmethod normal ((ring ring) pos)
  (nor ring))

(defmethod bounding-box ((ring ring))
  (with-vec (px py pz) (pos ring)
    (with-vec (nx ny nz) (normalize (nor ring))
      (let* ((r (sqrt (rmax2 ring)))
	     (rx (* r (- 1.0 nx)))
	     (ry (* r (- 1.0 ny)))
	     (rz (* r (- 1.0 nz))))
	(when (zerop rx)
	  (setf rx (/ r 1000)))
	(when (zerop ry)
	  (setf ry (/ r 1000)))
	(when (zerop rz)
	  (setf rz (/ r 1000)))
	(list
	 (vec (- px rx) (- py ry) (- pz rz))
	 (vec (+ px rx) (+ py ry) (+ pz rz)))))))

(defmacro with-cross-product ((ux uy uz) v w &body body)
  (cond
    ((symbolp v)
     (let ((vx (gensym)) (vy (gensym)) (vz (gensym)))
       `(with-vec (,vx ,vy ,vz) ,v
	 (with-cross-product (,ux ,uy ,uz) (,vx ,vy ,vz) w ,@body))))
    ((symbolp w)
     (let ((wx (gensym)) (wy (gensym)) (wz (gensym)))
       `(with-vec (,wx ,wy ,wz) ,w
	 (with-cross-product (,ux ,uy ,uz) ,v (,wx ,wy ,wz) ,@body))))
    (t
     (destructuring-bind (vx vy vz) v
       (destructuring-bind (wx wy wz) w
	 `(let ((,ux (- (* ,vy ,wz) (* ,vz ,wy)))
		(,uy (- (* ,vz ,wx) (* ,vx ,wz)))
		(,uz (- (* ,vx ,wy) (* ,vy ,wx))))
	    ,@body))))))

(defclass triangle (shape)
  ((p1 :initarg :p1 :accessor p1)
   (p2 :initarg :p2 :accessor p2)
   (p3 :initarg :p3 :accessor p3)
   (nor :initarg :normal :accessor nor)))

(defmethod initialize-instance :after ((triangle triangle) &rest initargs)
  (declare (ignore initargs))
  (with-vec (p1x p1y p1z) (p1 triangle)
    (with-vec (p2x p2y p2z) (p2 triangle)
      (with-vec (p3x p3y p3z) (p3 triangle)
	(let ((e1x (- p1x p2x))
	      (e1y (- p1y p2y))
	      (e1z (- p1z p2z))
	      (e2x (- p1x p3x))
	      (e2y (- p1y p3y))
	      (e2z (- p1z p3z)))
	  (with-cross-product (nx ny nz) (e1x e1y e1z) (e2x e2y e2z)
	    (setf (nor triangle) (if (= nx ny nz 0)
				     (vec 1 0 0)
				     (normalize (vec nx ny nz))))
	    triangle))))))

(defmethod intersect ((triangle triangle) ray)
  (with-vec (p1x p1y p1z) (p1 triangle)
    (with-vec (p2x p2y p2z) (p2 triangle)
      (with-vec (p3x p3y p3z) (p3 triangle)
	(let ((edge1x (- p2x p1x))
	      (edge1y (- p2y p1y))
	      (edge1z (- p2z p1z))
	      (edge2x (- p3x p1x))
	      (edge2y (- p3y p1y))
	      (edge2z (- p3z p1z)))
	  (with-vec (dx dy dz) (ray-dir ray)
	    (with-cross-product (pvecx pvecy pvecz)
				(dx dy dz)
				(edge2x edge2y edge2z)
	      (let ((det (+ (* edge1x pvecx)
			    (* edge1y pvecy)
			    (* edge1z pvecz))))
		(if (zerop det)
		    2e30
		    (with-vec (origx origy origz) (ray-pos ray)
		      (let ((inv-det (/ 1.0 det))
			    (tvecx (- origx p1x))
			    (tvecy (- origy p1y))
			    (tvecz (- origz p1z)))
			(let ((u (* (+ (* pvecx tvecx)
				       (* pvecy tvecy)
				       (* pvecz tvecz))
				    inv-det)))
			  (if (<= 0.0 u 1.0)
			      (with-cross-product (wx wy wz)
						  (tvecx tvecy tvecz)
						  (edge1x edge1y edge1z)
				(let ((v (* (+ (* dx wx) (* dy wy) (* dz wz))
					    inv-det)))
				  (if (and (>= v 0.0) (<= (+ u v) 1.0))
				      (let ((t1 (* (+ (* edge2x wx)
						      (* edge2y wy)
						      (* edge2z wz))
						   inv-det)))
					(if (minusp t1)
					    2e30
					    t1))
				      2e30)))
			      2e30)))))))))))))

(defmethod normal ((triangle triangle) pos)
  (nor triangle))

(defmethod bounding-box ((triangle triangle))
  (with-vec (p1x p1y p1z) (p1 triangle)
    (with-vec (p2x p2y p2z) (p2 triangle)
      (with-vec (p3x p3y p3z) (p3 triangle)
	(let* ((xmin (min p1x p2x p3x))
	       (ymin (min p1y p2y p3y))
	       (zmin (min p1z p2z p3z))
	       (xmax (max p1x p2x p3x))
	       (ymax (max p1y p2y p3y))
	       (zmax (max p1z p2z p3z))
	       (xdiff (- xmax xmin))
	       (ydiff (- ymax ymin))
	       (zdiff (- zmax zmin))
	       (maxdiff (max xdiff ydiff zdiff)))
	  (when (zerop xdiff)
	    (incf xmax (/ maxdiff 1000))
	    (decf xmin (/ maxdiff 1000)))
	  (when (zerop ydiff)
	    (incf ymax (/ maxdiff 1000))
	    (decf ymin (/ maxdiff 1000)))
	  (when (zerop zdiff)
	    (incf zmax (/ maxdiff 1000))
	    (decf zmin (/ maxdiff 1000)))
	  (list (vec xmin ymin zmin) (vec xmax ymax zmax)))))))

(defclass pipe (shape)
  ((p1 :initarg :p1 :accessor p1)
   (p2 :initarg :p2 :accessor p2)
   (r1 :initarg :r1 :accessor r1)
   (r2 :initarg :r2 :accessor r2)))

(defmethod intersect ((pipe pipe) ray)
  (with-vec (ox oy oz) (ray-pos ray)
    (with-vec (dx dy dz) (ray-dir ray)
      (with-vec (p1x p1y p1z) (p1 pipe)
	(with-vec (p2x p2y p2z) (p2 pipe)
	  (decf ox p1x)
	  (decf oy p1y)
	  (decf oz p1z)
	  (let ((ax (- p2x p1x))
		(ay (- p2y p1y))
		(az (- p2z p1z)))
	    (let ((aa (+ (* ax ax) (* ay ay) (* az az)))
		  (ad (+ (* ax dx) (* ay dy) (* az dz)))
		  (ao (+ (* ax ox) (* ay oy) (* az oz))))
	      (let ((ad/aa (/ ad aa))
		    (ao/aa (/ ao aa)))
		(let ((bx (- dx (* ad/aa ax)))
		      (by (- dy (* ad/aa ay)))
		      (bz (- dz (* ad/aa az)))
		      (cx (- ox (* ao/aa ax)))
		      (cy (- oy (* ao/aa ay)))
		      (cz (- oz (* ao/aa az))))
		  (let* ((b2 (+ (* bx bx) (* by by) (* bz bz)))
			 (bc (+ (* bx cx) (* by cy) (* bz cz)))
			 (c2 (+ (* cx cx) (* cy cy) (* cz cz)))

			 (r1 (the fast-float (r1 pipe)))
			 (r2 (the fast-float (r2 pipe)))
			 (dr (the fast-float (- r2 r1)))
			 (e (* dr ad/aa))
			 (f (+ (* dr ao/aa) r1))
			 (b2 (- b2 (* e e)))
			 (bc (- bc (* e f)))
			 (bc/b2 (/ bc b2))
			 (r2-c2/b2 (/ (- (* f f) c2) b2))

			 (g (+ (* bc/b2 bc/b2) r2-c2/b2)))
		    (if (minusp g)
			2e30
			(let* ((sqrt-g (sqrt g))
			       (t1 (- (+ sqrt-g bc/b2)))
			       (x1 (+ ao/aa (* t1 ad/aa))))
			  ;;(format t "~&t1 = ~A, x1 = ~A~%" t1 x1)
			  (if (and (>= t1 0.0) (<= 0.0 x1 1.0))
			      t1
			      (let* ((t2 (- sqrt-g bc/b2))
				     (x2 (+ ao/aa (* t2 ad/aa))))
				;;(format t "~&t2 = ~A, x2 = ~A~%" t2 x2)
				(if (and (>= t2 0.0) (<= 0.0 x2 1.0))
				    t2
				    2e30)))))))))))))))

(defmethod normal ((pipe pipe) pos)
  (with-vec (px py pz) pos
    (with-vec (p1x p1y p1z) (p1 pipe)
      (with-vec (p2x p2y p2z) (p2 pipe)
	(let ((ax (- p2x p1x))
	      (ay (- p2y p1y))
	      (az (- p2z p1z)))
	  (decf px p1x)
	  (decf py p1y)
	  (decf pz p1z)
	  (let* ((aa (+ (* ax ax) (* ay ay) (* az az)))
		 (1/aa (/ 1.0 aa))
		 (r1 (the fast-float (r1 pipe)))
		 (r2 (the fast-float (r2 pipe)))
		 (dr (- r2 r1))
		 (u (* (+ aa (* dr dr)) 1/aa 1/aa))
		 (v (* dr r1 1/aa))
		 (axpx (* ax px))
		 (aypy (* ay py))
		 (azpz (* az pz))
		 (nx (- (* px (- 1.0 (* u (* ax ax))))
			(* ax (+ v (* u (+ aypy azpz))))))
		 (ny (- (* py (- 1.0 (* u (* ay ay))))
			(* ay (+ v (* u (+ axpx azpz))))))
		 (nz (- (* pz (- 1.0 (* u (* az az))))
			(* az (+ v (* u (+ axpx aypy)))))))
	    (normalize (vec nx ny nz))))))))

(defmethod bounding-box ((pipe pipe))
  (with-slots (r1 r2) pipe
    (with-vec (p1x p1y p1z) (p1 pipe)
      (with-vec (p2x p2y p2z) (p1 pipe)
	(list (vec (- (min p1x p2x) r1)
		   (- (min p1y p2y) r1)
		   (- (min p1z p2z) r1))
	      (vec (+ (max p1x p2x) r1)
		   (+ (max p1y p2y) r1)
		   (+ (max p1z p2z) r1)))))))

(defmethod bounding-box ((objects list))
  (let ((minx 1e30)
	(miny 1e30)
	(minz 1e30)
	(maxx -1e30)
	(maxy -1e30)
	(maxz -1e30))
    (dolist (object objects)
      (destructuring-bind (min max) (bounding-box object)
	(with-vec (obj-min-x obj-min-y obj-min-z) min
	  (with-vec (obj-max-x obj-max-y obj-max-z) max
	    (setf minx (min minx obj-min-x))
	    (setf miny (min miny obj-min-y))
	    (setf minz (min minz obj-min-z))
	    (setf maxx (max maxx obj-max-x))
	    (setf maxy (max maxy obj-max-y))
	    (setf maxz (max maxz obj-max-z))))))
    (list (vec minx miny minz)
	  (vec maxx maxy maxz))))

(defun list-intersect (objects ray hit)
  (let ((t1 1e30)
	(object nil))
    (declare (fast-float t1))
    (dolist (obj objects)
      (let ((t2 (intersect obj ray)))
	(declare (fast-float t2))
	(when (< t2 t1)
	  (setf t1 t2)
	  (setf object obj))))
    (hitpoint hit ray t1)
    object))

(defun reflection (ray pos normal bump)
  (declare (type vec pos normal)
	   (type function bump))
  (with-vec (dx dy dz) (ray-dir ray)
    (with-vec (nx ny nz) normal
      (let* ((a (* 2.0 (+ (* nx dx) (* ny dy) (* nz dz))))
	     (d2x (- dx (* a nx)))
	     (d2y (- dy (* a ny)))
	     (d2z (- dz (* a nz))))
	(make-ray pos (funcall bump (vec d2x d2y d2z) pos) (ray-nm ray))))))

#|
	Vacuum		1.0
	Air		1.0003
	Ice		1.31
	Water		1.333
	Glass		1.5
	Diamond		2.419
|#
(defun refraction (ray pos normal n1 n2)
  (declare (optimize (debug 3)))
  (declare (type vec pos normal)
	   (fast-float n1 n2))
  (with-vec (dx dy dz) (ray-dir ray)
    (setf dx (- dx))
    (setf dy (- dy))
    (setf dz (- dz))
    (with-vec (nx ny nz) normal
      (let ((nd (+ (* nx dx) (* ny dy) (* nz dz))))
	;;(format t "~&ray = ~A~%" ray)
	;;(format t "~&normal = ~A~%" normal)
	;;(format t "~&nd = ~A~%" nd)
	(when (minusp nd)
	  (setf nd (- nd))
	  (setf nx (- nx))
	  (setf ny (- ny))
	  (setf nz (- nz))
	  (rotatef n1 n2))
	(let* ((n1/n2 (/ n1 n2))
	       (a (- 1.0 (* n1/n2 n1/n2 (- 1.0 (* nd nd))))))
	  ;;(break)
	  ;;(format t "a = ~A~%" a)
	  (unless (minusp a)
	    (let* ((b (- (* n1/n2 nd) (sqrt a)))
		   (d2 (vec (- (* b nx) (* n1/n2 dx))
			    (- (* b ny) (* n1/n2 dy))
			    (- (* b nz) (* n1/n2 dz)))))
	      ;;(format t "~&d2 = ~A~%" d2)
	      ;;(break)
	      (make-ray pos d2 (ray-nm ray)))))))))
#+(or)
(defun refraction (ray pos normal n1 n2)
  (declare (optimize (debug 3)))
  (declare (type vec pos normal)
	   (fast-float n1 n2))
  (with-vec (dx dy dz) (ray-dir ray)
    (with-vec (nx ny nz) normal
      (let ((nd (+ (* nx dx) (* ny dy) (* nz dz))))
	(format t "~&ray = ~A~%" ray)
	(format t "~&normal = ~A~%" normal)
	(format t "~&nd = ~A~%" nd)
	(when (minusp nd)
	  (rotatef n1 n2))
	(let* ((n1/n2 (/ n1 n2))
	       (a (+ (* nd nd) (* n1/n2 n1/n2) -1.0)))
	  ;(break)
	  (format t "a = ~A~%" a)
	  (unless (minusp a)
	    (let* ((b (- (sqrt a) nd))
		   (d2 (vec (* n1/n2 (+ (* b nx) dx))
			    (* n1/n2 (+ (* b ny) dy))
			    (* n1/n2 (+ (* b nz) dz)))))
	      (format t "~&d2 = ~A~%" d2)
	      ;(break)
	      (make-ray pos (normalize d2) (ray-nm ray)))))))))

(defun roughness (x)
  (declare (fast-float x))
  (let ((2x (* 2.0 x)))
    (lambda (normal point)
      (declare (ignore point))
      (flet ((foo () (- (random 2x) x)))
	(incf (vec-x normal) (foo))
	(incf (vec-y normal) (foo))
	(incf (vec-z normal) (foo))
	(normalize normal)))))

(defmacro with-random-sphere-point ((x y z &optional r1 r2) &body body)
  (let ((a (gensym))
	(b (gensym))
	(c (gensym))
	(d (gensym)))
    (declare (ignorable b))
    ;;#+(or)
    `(progn
       ;(unless ,r1 (setf ,r1 (random 1.0)))
       ;(unless ,r2 (setf ,r2 (random 1.0)))
      (let ((,a (random 1.0))
	    (,z (- (* 2.0 (random 1.0)) 1.0)))
	(declare (type (fast-float 0.0 1.0) ,a))
	(declare (type (fast-float -1.0 1.0) ,z))
	(let ((,c (* 2pi ,a))
	      (,d (the fast-float (sqrt (- 1.0 (* ,z ,z))))))
	  (declare (type fast-float ,c ,d))
	  (let ((,x (* ,d (cos ,c)))
		(,y (* ,d (sin ,c))))
	    (declare (type fast-float ,x ,y))
	    ,@body))))
    #+(or)
    `(let ((,a (random 2pi))
	   (,b (- (acos (random 1.0)) #.(coerce (* .5 pi) 'fast-float))))
       (declare (fast-float ,a ,b))
       (let ((,x (* (cos ,a) (cos ,b)))
	     (,y (* (sin ,a) (cos ,b)))
	     (,z (sin ,b)))
	 (declare (fast-float ,x ,y ,z))
	 ,@body))))

(defun uniform-bounce (pos normal)
  (declare (type vec pos normal))
  (with-vec (nx ny nz) normal
    (with-random-sphere-point (dx dy dz)
      (when (minusp (+ (* dx nx) (* dy ny) (* dz nz)))
	(setf dx (- dx))
	(setf dy (- dy))
	(setf dz (- dz)))
      (make-ray pos (vec dx dy dz) 0.0))))

(defmacro with-orthogonal-vector ((x1 y1 z1) (x2 y2 z2) &body body)
  (let ((gb (gensym))
	(gx (gensym)) (gy (gensym)) (gz (gensym))
	(ax (gensym)) (ay (gensym)) (az (gensym)))
    `(flet ((,gb (,x1 ,y1 ,z1) ,@body))
       (let ((,gx ,x2) (,gy ,y2) (,gz ,z2))
	 (let ((,ax (abs ,gx)) (,ay (abs ,gy)) (,az (abs ,gz)))
	   (if (> ,ax ,ay)
	       (if (> ,ay ,az)
		   (,gb ,gy (- ,gx) 0.0)
		   (,gb ,gz 0.0 (- ,gx)))
	       (if (> ,ax ,az)
		   (,gb ,gy (- ,gx) 0.0)
		   (,gb 0.0 ,gz (- ,gy)))))))))

(defmacro with-normalized-vector ((x1 y1 z1) (x2 y2 z2) &body body)
  (let ((q (gensym)))
    `(let ((,q (/ 1.0 (sqrt (+ (* ,x2 ,x2) (* ,y2 ,y2) (* ,z2 ,z2))))))
       (let ((,x1 (* ,q ,x2))
	     (,y1 (* ,q ,y2))
	     (,z1 (* ,q ,z2)))
	 ,@body))))

(defun cosine-bounce (ray pos normal &optional r1 r2)
  (declare (type vec pos normal)
	   (type ray ray))
  (with-vec (nx ny nz) normal
    (with-vec (dx dy dz) (ray-dir ray)
      (when (plusp (+ (* nx dx) (* ny dy) (* nz dz)))
	(setf nx (- nx))
	(setf ny (- ny))
	(setf nz (- nz))))
    (with-orthogonal-vector (ux uy uz) (nx ny nz)
      (with-normalized-vector (u2x u2y u2z) (ux uy uz)
	(with-cross-product (vx vy vz) (nx ny nz) (u2x u2y u2z)
	  #+(or)
	  (flet ((len (x y z) (sqrt (+ (* x x) (* y y) (* z z)))))
	    (format t "~&~,3F ~,3F ~,3F~%"
		    (len nx ny nz)
		    (len u2x u2y u2z)
		    (len vx vy vz)))
	  (let ((r1 (* 2pi (the fast-float (or r1 (random 1.0)))))
		(r2 (or r2 (random 1.0))))
	    (declare (type (fast-float 0.0 #.2pi) r1)
		     (type (fast-float 0.0 1.0) r2))
	    (let* ((x (sqrt r2))
		   (b (the fast-float (sqrt (- 1.0 r2))))
		   (y (* b (cos r1)))
		   (z (* b (sin r1))))
	      (setf (ray-pos ray) pos)
	      (setf (vec-x (ray-dir ray)) (+ (* x nx) (* y u2x) (* z vx)))
	      (setf (vec-y (ray-dir ray)) (+ (* x ny) (* y u2y) (* z vy)))
	      (setf (vec-z (ray-dir ray)) (+ (* x nz) (* y u2z) (* z vz)))
	      nil)))))))
	
(defun hitpoint (hit ray t1)
  (declare (fast-float t1))
  (with-vec (px py pz) (ray-pos ray)
    (with-vec (dx dy dz) (ray-dir ray)
      (let ((t2 (* t1 .999999)))
	(setf (vec-x hit) (+ px (* t2 dx))
	      (vec-y hit) (+ py (* t2 dy))
	      (vec-z hit) (+ pz (* t2 dz)))
	nil
	#+(or)
	(vec (+ px (* t2 dx))
	     (+ py (* t2 dy))
	     (+ pz (* t2 dz)))))))

(defvar *depth* 0)
(declaim (fixnum *depth*))

(defvar *rays* 0)
(defvar *primary-rays* 0)
(defvar *shadow-rays* 0)
(declaim (integer *rays* *primary-rays* *shadow-rays*))

(defmacro 1+/fixnum (x)
  `(locally
     (declare (optimize (speed 3) (safety 0) (debug 0)))
     (the fixnum (1+ (the fixnum ,x)))))

(declaim (ftype (function (box ray) (value null)) sky))
(defun sky (result ray)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (box result))
  (let ((dir (ray-dir ray))
	(nm (ray-nm ray)))
    (declare (ignorable dir nm))
    (setf (aref result)
	  (with-vec (x y z) dir
	    (declare (ignorable x y z))
	    (let* ((v (+ (* #.(/ (sqrt 2.0)) x) (* #.(/ (sqrt 2.0)) y)))
		   (w (+ (* .5 v) .5)))
	      (declare (type (fast-float 0.0) w))
	      (max 0.0 (+ -0.2 (* 1.1 (the fast-float (expt w 1.0)))))))))
  nil)

(declaim (ftype (function (fast-float fast-float fast-float fast-float)
			  (value fast-float))
		dieletric-fresnel))
(defun dieletric-fresnel (cos1 cos2 n1 n2)
  (declare (fast-float cos1 cos2 n1 n2))
  (when (minusp cos1)
    (setf cos1 (- cos1))
    (rotatef n1 n2))
  (let ((a (* n1 cos1))
	(b (* n2 cos2))
	(c (* n2 cos1))
	(d (* n1 cos2)))
    (let ((r-per (/ (- a b) (+ a b)))
	  (r-par (/ (- c d) (+ c d))))
      (* .5 (+ (* r-par r-par) (* r-per r-per))))))

(defun distance2 (p1 p2)
  (with-vec (p1x p1y p1z) p1
    (with-vec (p2x p2y p2z) p2
      (flet ((sqr (x) (* x x)))
	(+ (sqr (- p1x p2x))
	   (sqr (- p1y p2y))
	   (sqr (- p1z p2z)))))))

(defun distance (p1 p2)
  (declare (inline distance2))
  (sqrt (distance2 p1 p2)))

(declaim (ftype (function (box ray t list t) (value null)) raytrace))
(defun raytrace (result ray objects samples direct-lighting-p)
  (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (declare (box result))
  (incf *rays*)
  (let ((*depth* (1+/fixnum *depth*)))
    (when (> *depth* 100)
      (format t "~&Deep path.~%")
      (setf (aref result) 0.0)
      (return-from raytrace nil))
    (let* ((hit (vec 0 0 0))
	   (object (kd-intersect objects ray hit))
	   (path-sample (get-next-sample samples))
	   (light-sample (get-next-sample samples)))
      (declare (vec hit))
      (declare (dynamic-extent hit))
      (if object
	  (let* ((m1 (material object))
		 (m (if (functionp m1) (funcall m1 hit) m1))
		 (diffuse (diffuse m))
		 (diffuse (if (functionp diffuse)
			      (funcall diffuse (ray-nm ray))
			      diffuse))
		 (specular (specular m))
		 (transmission (transmission m))
		 (r (random 1.0))
		 (n (normal object hit))
		 (transmitted-ray nil))
	    (declare (fast-float diffuse specular transmission r))

	    (when (plusp transmission)
	      (with-vec (dx dy dz) (ray-dir ray)
		(let ((hit2 (vec (+ (vec-x hit) (* .1 dx))
				 (+ (vec-y hit) (* .1 dy))
				 (+ (vec-z hit) (* .1 dz))))
		      (n1 (funcall (the function (n1 m)) (ray-nm ray)))
		      (n2 (funcall (the function (n2 m)) (ray-nm ray))))
		  (setf transmitted-ray
			(refraction ray hit2 (normal object hit) n1 n2))
		  (if transmitted-ray
		      (with-vec (nx ny nz) n
			(with-vec (tx ty tz) (ray-dir transmitted-ray)
			  (with-vec (dx dy dz) (ray-dir ray)
			    (let ((r (* transmission 
					(dieletric-fresnel
					 (- (+ (* nx dx)
					       (* ny dy)
					       (* nz dz)))
					 (abs
					  (+ (* nx tx)
					     (* ny ty)
					     (* nz tz)))
					 n1 n2))))
			      (setf specular (+ specular r)
				    transmission (- transmission r))))))
		      ;; Total reflection.
		      (setf specular (+ specular transmission)
			    transmission 0.0)))))

	    (cond
	      ((< r (if (<= *depth* 3)
			(if (plusp diffuse) 1.0 0.0)
			diffuse))
	       ;;Direct lighting.
	       ;;#+(or)
	       (let* ((r1 (car path-sample))
		      (r2 (cdr path-sample)))
		 (cosine-bounce ray hit n r1 r2)
		 (raytrace result ray objects samples nil)
		 (setf (aref result)
		       (* (if (<= *depth* 3) diffuse 1.0)
			  (+
			   (aref result)
			   ;;#+(or)
			   (flet ((random-point-on-disc ()
				    (let* ((r1 (* 200.0 (the fast-float (car light-sample))))
					   (r2 (* 2pi (the fast-float (cdr light-sample))))
					   ;;(r1 (* 200.0 (sqrt (random 1.0))))
					   ;;(r2 (random 2pi))
					   (ax 0.0) (ay 0.0) (az 1.0)
					   (bx (- (/ (sqrt 1.0))))
					   (by (/ (sqrt 1.0)))
					   (bz 0.0)
					   (r1cosr2 (* r1 (cos (the (fast-float 0.0 #.2pi) r2))))
					   (r1sinr2 (* r1 (sin (the (fast-float 0.0 #.2pi) r2)))))
				      (vec (+ -1000.0
					      (* r1cosr2 ax)
					      (* r1sinr2 bx))
					   (+ 1000.0
					      (* r1cosr2 ay)
					      (* r1sinr2 by))
					   (+ 500.0
					      (* r1cosr2 az)
					      (* r1sinr2 bz))))))
			     (let* ((p3 (random-point-on-disc))
				    (hit2 (vec 0 0 0))
				    (dir2 (normalize
					   (vec (- (vec-x p3) (vec-x hit))
						(- (vec-y p3) (vec-y hit))
						(- (vec-z p3) (vec-z hit))))))
			       (incf *shadow-rays*)
			       (if (eq (kd-intersect objects
						     (make-ray hit dir2 0.0)
						     hit2)
				       (first objects))
				   (with-vec (nx ny nz) n
				     (with-vec (n2x n2y n2z) (normal (first objects) hit2)
				       (/ (* (the fast-float (emit (material (first objects))))
					     (abs (+ (* nx (vec-x dir2))
						     (* ny (vec-y dir2))
						     (* nz (vec-z dir2))))
					     (abs (+ (* n2x (vec-x dir2))
						     (* n2y (vec-y dir2))
						     (* n2z (vec-z dir2))))
					     200.0 200.0 pi)
					  (the fast-float (distance2 hit p3))
					  pi)))
				   0.0)))))))
	       ;;No direct lighting.
	       #+(or)
	       (let ((b (uniform-bounce hit n)))
		 (with-vec (nx ny nz) n
		   (with-vec (bx by bz) (ray-dir b)
		     (* 2.0
			(+ (* nx bx) (* ny by) (* nz bz))
			(raytrace b objects t))))))
	      ((< diffuse r (+ diffuse specular))
	       (raytrace result (reflection ray hit n (bump m))
			 objects samples t)
	       (when direct-lighting-p
		 (incf (aref result) (the fast-float (emit m)))))
	      ((< (+ diffuse specular) r (+ diffuse specular transmission))
	       (raytrace result transmitted-ray objects samples t)
	       (when direct-lighting-p
		 (incf (aref result) (the fast-float (emit m)))))
	      (t
	       (setf (aref result)
		     (if direct-lighting-p
			 (the fast-float (emit m))
			 0.0)))))
	  (sky result ray))))
  nil)

(defun normalize (vec)
  (with-vec (x y z) vec
    (let ((q (/ 1.0 (sqrt (+ (* x x) (* y y) (* z z))))))
      (declare (fast-float q))
      (setf (vec-x vec) (* q x))
      (setf (vec-y vec) (* q y))
      (setf (vec-z vec) (* q z))
      vec)))

(defvar *random-offset*)

(defun sample-wavelength (ray x)
  (declare (type (fast-float 0.0 1.0) x))
  (multiple-value-bind (nm weight)
      (typecase x
	;; 2% probability for 40/8.5%
	((real 0.00 0.02)	(values (+ 360 (* 40 (/ x 0.02)))
					(/ 40 470 .02)))
	;; 93% probability for 300/63.8%
	((real 0.02 0.95)	(values (+ 400 (* 300 (/ (- x 0.02) 0.95)))
					(/ 400 470 .93)))
	;; 5% probability for 130/27.7%
	((real 0.95 1.00)	(values (+ 700 (* 130 (/ (- x 0.95) 0.05)))
					(/ 130 470 .05))))
    (setf (ray-nm ray) nm)
    (setf (ray-weight ray) weight)
    nil))

(defun eye-ray (ray x y w h samples)
  (setf y (- h y))
  (setf (ray-pos ray) (vec 50.0 52.0 295.6))
  (let ((sample (get-next-sample samples))
	(rd (normalize (vec 0.0 -0.042612 -1.0)))
	(cxx (/ (* .5135 w) h)))
    (declare (ignore sample))
    (with-cross-product (cyx cyy cyz) (cxx 0.0 0.0) rd
      (let* ((u (- (/ x w) .5))
	     (v (- (/ y h) .5))
	     (rdx (+ (* cxx u) (* cyx v) (vec-x rd)))
	     (rdy (+           (* cyy v) (vec-y rd)))
	     (rdz (+           (* cyz v) (vec-z rd))))
	(incf (vec-x (ray-pos ray)) (* 140.0 rdx))
	(incf (vec-y (ray-pos ray)) (* 140.0 rdy))
	(incf (vec-z (ray-pos ray)) (* 140.0 rdz))
	(setf (ray-dir ray) (normalize (vec rdx rdy rdz)))
	(sample-wavelength ray (the fast-float (car (get-next-sample samples))))
	nil))))

(defun eye-ray (ray x y w h samples)
  (declare (optimize (speed 3) (space 2) (debug 0) (safety 0)))
  (declare (ray ray))
  (declare (image-index x y))
  ;;#+(or)
  (incf *primary-rays*)
  (setf y (- h y))
  (setf (vec-x (ray-pos ray)) 0.0)
  (setf (vec-y (ray-pos ray)) 0.0)
  (setf (vec-z (ray-pos ray)) -1000.0)
  ;;Depth of field.
  (let ((u (* 2.0 (sqrt (random 1.0))))
	(v (* 2pi (random 1.0))))
    (incf (vec-x (ray-pos ray)) (* u (cos v)))
    (incf (vec-y (ray-pos ray)) (* u (sin v))))
  (let* ((sample (get-next-sample samples))
	 (r1 (car sample))
	 (r2 (cdr sample))
	 (rd (vec (- x (* .5 w) (vec-x (ray-pos ray)) (the fast-float r1))
		  (- y (* .5 h) (vec-y (ray-pos ray)) (the fast-float r2))
		  (- 0 (vec-z (ray-pos ray))))))
    ;(declare (type vec rd) (dynamic-extent rd))
    (setf (ray-dir ray) (normalize rd))
    (sample-wavelength ray (the fast-float (car (get-next-sample samples))))
    nil))

(defun shuffle (list)
  (do ((length (length list) (1- length))
       (current list (rest current)))
      ((< length 2) list)
    (rotatef (first current) (elt current (random length)))))

(defun shuffle-vector (vector)
  (declare (type (array t (*)) vector))
  (loop with n = (array-dimension vector 0)
	for i from 0 below n
	and m from n downto 0 do
	(rotatef (aref vector i) (aref vector (+ i (random m)))))
  vector)

(defun shuffle2 (array)
  (declare (type (simple-array fast-float (*)) array))
  (let ((n (truncate (length array) 2)))
    (declare (type index n))
    (dotimes (i n array)
      (let ((j (* 2 (random n))))
	(declare (index i j))
	(rotatef (aref array i) (aref array j))
	(rotatef (aref array (1+ i)) (aref array (1+ j)))))))

(defun inverse (x n)
  (loop for i below n
	for y = 1 then (* 2 y)
	sum (if (logbitp (- n i 1) x) y 0)))

(defun halton-sequence (n)
  (flet ((inverse (x)
	   (/ (loop for i below n
		    for y = 1 then (* 2 y)
		    sum (if (logbitp (- n i 1) x) y 0))
	      (expt 2 n))))
    (loop for i below (expt 2 n)
	  collect (float (inverse i) 1.0))))

(defun hammersley-samples (n)
  (let ((m (round (log n 2)))
	(r1 0 #+(or)(random (1- n)))
	(r2 0 #+(or)(random (1- n))))
    (flet ((inverse (x)
	     (/ (logxor (loop for i below m
			      for y = 1 then (* 2 y)
			      sum (if (logbitp (- m i 1) x) y 0))
			r1)
		n)))
      (loop for i below n collect
	    (cons (float (/ (logxor i r2) n) 1.0)
		  (float (inverse i) 1.0))))))

(defun randomized-hammersley-sampler (n)
  (declare (type (integer 0 100000) n))
  (let ((m (round (log n 2)))
	(1/n (/ 1.0 n)))
    (declare (type (integer 1 32) m))
    (lambda (ignore)
      (declare (ignore ignore))
      (let ((r1 (random n))
	    (r2 (random n)))
	(flet ((inverse (x)
		 (declare (fixnum x))
		 (the (unsigned-byte 32)
		   (loop for i below m
			 for y fixnum = 1 then (* 2 y)
			 sum (if (logbitp (- m i 1) x) y 0)))))
	  (loop for i below n collect
		(cons (* 1/n (logxor i r1))
		      (* 1/n (logxor (inverse i) r2)))))))))

(defun folded-radical-inverse (n b)
  (let* ((val 0.0)
	 (inv-base (/ 1.0 b))
	 (ib inv-base)
	 (mod-offset 0))
    (loop until (= (+ val (* b ib)) val) do
	  (let ((digit (mod (+ n mod-offset) b)))
	    (incf val (* digit ib))
	    (setf n (truncate n b))
	    (setf ib (* ib inv-base))
	    (incf mod-offset)))
    val))

(defun hammersley-zaremba-samples (n)
  (loop for i below n collect
	(cons (float (/ i n) 1.0)
	      (float (folded-radical-inverse i 2) 1.0))))

(defun multi-jittering-sampler (n)
  (let* ((n (isqrt n))
	 (delta (/ 1.0 n))
	 (epsilon (/ delta n)))
    (lambda (m)
      (declare (ignore m))
      (flet ((samples ()
	       (shuffle (loop for i below n collect
			      (+ (* epsilon i) (random epsilon))))))
	(let ((xs (loop repeat n collect (samples)))
	      (ys (loop repeat n collect (samples))))
	  (shuffle
	   (loop for i below n append
		 (loop for j below n collect
		       (cons (+ (* delta i) (pop (elt xs i)))
			     (+ (* delta j) (pop (elt ys j))))))))))))

(defun stratified-samples (n)
  (let* ((n (isqrt n))
	(delta (/ 1.0 n)))
    (shuffle
     (loop for i below n append
	   (loop for j below n collect
		 (cons (+ (* delta i) (random delta))
		       (+ (* delta j) (random delta))))))))

(defun stratified-sampler (n1)
  (let* ((n (isqrt n1))
	 (delta (/ 1.0 n)))
    (lambda (m)
      (declare (ignore m))
      (let ((index 0)
	    (vector (make-array n1 :fill-pointer n1)))
	(loop for i below n do
	      (loop for j below n do
		    (setf (aref vector index)
			  (cons (+ (* delta i) (random delta))
				(+ (* delta j) (random delta))))
		   (incf index)))
	(shuffle-vector vector)))))

(defun latin-hypercube-samples (n)
  (flet ((samples ()
	   (shuffle
	    (loop with delta = (/ 1.0 n)
		  for i from 0 below n
		  collect (+ (* delta i) (random delta))))))
    (mapcar #'cons (samples) (samples))))

(defun uniform-samples (n)
  (flet ((samples ()
	   (loop repeat n collect (random 1.0))))
    (mapcar #'cons (samples) (samples))))

(defun draw-samples (fn n)
  (with-window (:width 400 :height 400)
    (loop for (x . y) in (funcall fn n) do
	  (flet ((draw-cross (x y &optional (size 2))
		   (draw-line (- x size) y (+ x size 1) y)
		   (draw-line x (- y size) x (+ y size 1))))
	    (draw-cross (round (* 400 x)) (round (* 400 y)))))))

(defun precomputed-sampler (fn n)
  (let ((list (funcall fn n)))
    (lambda (m)
      (declare (ignore m))
      (copy-list list))))

(defun offset-sampler (fn)
  (lambda (n)
    (mapcar (lambda (x)
	      (flet ((offset (x y) (mod (+ x y) 1.0)))
		(cons (offset (car x) (car *random-offset*))
		      (offset (cdr x) (cdr *random-offset*)))))
	    (funcall fn n))))

(defun generate-samples (fn n m)
  (let ((samples
	 (list*
	  ;; Normal sample for pixel position.
	  (funcall fn n)
	  ;; Wavelength sample; doesn't need y.
	  (let ((s (make-array n :fill-pointer n))
		(delta (/ 1.0 n)))
	    (loop for i from 0 below n do
		  (setf (aref s i)
			(cons (+ (* delta i) (random delta))
			      0.0)))
	    (shuffle-vector s))
	  ;; More samples.
	  (loop repeat (- m 2) collect (funcall fn n)))))
    (cons samples samples)))

(defun reset-pointer (samples)
  (setf (cdr samples) (car samples)))

(defun get-next-sample (samples)
  (if (null (rest samples))
      (cons (random 1.0) (random 1.0))
      (prog1 (vector-pop (first (rest samples)))
	(pop (rest samples)))))

(defun make-queue ()
  (let ((q (list nil)))
    (cons q q)))

(defun queue-push (x q)
  (setf (cdr q) (setf (cddr q) (list x))))

(defun queue-pop (q)
  (car (setf (car q) (cdar q))))

(declaim (ftype (function (box fast-float t) (value null))
		%wavelength-response))
(defun %wavelength-response (result wavelength table)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type box result)
	   (type (fast-float 360.0 830.0) wavelength)
	   (type #.(type-of +cie-1931-x+) table))
  (let ((i (floor wavelength))
	(j (floor (1+ wavelength))))
    (setf (aref result) (+ (* (- 1 (- wavelength i)) (aref table (- i 360)))
			   (* (- wavelength i) (aref table (- j 360)))))
    nil))

(defun wavelength-response (type wavelength)
  (etypecase wavelength
    ((real 360 830)	(let ((result (make-box)))
			  (%wavelength-response
			   result
			   (coerce wavelength 'fast-float)
			   (getf ;;#+(or)
				 `(:x ,+cie-1964-x+
				   :y ,+cie-1964-y+
				   :z ,+cie-1964-z+)
				 #+(or)
				 `(:x ,+cie-1931-x+
				   :y ,+cie-1931-y+
				   :z ,+cie-1931-z+)
				 type))
			  (aref result)))
    ((real 0 *)		0)))

(defun srgb-luminosity (r g b)
  (+ (* 0.212671 r) (* 0.715160 g) (* 0.072169 b)))

(defmacro with-preserved-luminosity ((r g b) &body body)
  (let ((x (gensym))
	(y (gensym)))
    `(let ((,y (srgb-luminosity ,r ,g ,b)))
       (multiple-value-prog1 (locally ,@body)
	 (if (zerop ,y)
	     (setf ,r 0 ,g 0 ,b 0)
	     (let ((,x (/ ,y (srgb-luminosity ,r ,g ,b))))
	       (setf ,r (* ,x ,r))
	       (setf ,g (* ,x ,g))
	       (setf ,b (* ,x ,b))))))))

(defun render (array objects &key (rays-per-pixel 1))
  (declare (optimize (speed 3) (space 2) (debug 0) (safety 0)))
  (declare (fixnum rays-per-pixel))
  (declare (type (simple-array fast-float (* *)) array))
  (let ((sampler
	 ;;(precomputed-sampler #'hammersley-zaremba-samples rays-per-pixel)
	 ;;(precomputed-sampler #'hammersley-samples rays-per-pixel)
	 ;;(offset-sampler (precomputed-sampler #'hammersley-samples rays-per-pixel))
	 ;;(randomized-hammersley-sampler rays-per-pixel)
	 ;;#'hammersley-samples
	 ;;(multi-jittering-sampler rays-per-pixel)
	 (stratified-sampler rays-per-pixel)
	 ;;#'stratified-samples
	 ;;#'latin-hypercube-samples
	 ;;#'uniform-samples
	 )
	(width (/ (array-dimension array 0) 3))
	(height (array-dimension array 1)))
    (declare (type image-index width height))

    (let* ((works 0)
	   (work (loop for y of-type image-index below height by 20 nconc
		      (loop for x of-type image-index below width by 20
			    do (incf (the fixnum works))
			    collect (list x y (+ x 20) (+ y 20)))))
	   (work-mutex (sb-thread:make-mutex))
	   (output (make-queue))
	   (output-mutex (sb-thread:make-mutex))
	   (output-sem (sb-thread:make-semaphore :count 0))
	   (stream *standard-output*)
	   (workers (loop for n below 4 collect
			  (sb-thread:make-thread
			   (lambda ()
			     (let ((*random-state* (make-random-state t))
				   (*standard-output* stream)
				   (result (make-box))
				   (ray (make-ray (vec 0 0 0) (vec 0 0 0) 0.0)))
			       (declare (type box result))
			       (let ((x0 0) (y0 0) (x1 0) (y1 0))
				 (declare (type image-index x0 y0 x1 y1))
				 (loop
				    (sb-thread:with-mutex (work-mutex)
				      (when (null work)
					(return))
				      (let ((w (pop work)))
					(setq x0 (first w))
					(setq y0 (second w))
					(setq x1 (third w))
					(setq y1 (fourth w))))
       (loop for y from y0 below y1 do
	     (loop for x from x0 below x1 do
		   (loop with samples = (generate-samples
					 sampler rays-per-pixel 6)
			 with cie-x of-type fast-float = 0.0
			 with cie-y of-type fast-float = 0.0
			 with cie-z of-type fast-float = 0.0
			 with 1/rpp of-type fast-float
			 = (/ 4.0 rays-per-pixel)
			 repeat rays-per-pixel do
			 (let ((*depth* 0))
			   (eye-ray ray x y width height samples)
			   (raytrace result
				     ray
				     objects
				     samples
				     t)
			   (format t "[~,2F]" (aref result))
			   (reset-pointer samples)
			   (let ((q (* (aref result) (ray-weight ray)))
				 (nm (ray-nm ray)))
			     (%wavelength-response result nm +cie-1931-x+)
			     (incf cie-x (* q (aref result)))
			     (%wavelength-response result nm +cie-1931-y+)
			     (incf cie-y (* q (aref result)))
			     (%wavelength-response result nm +cie-1931-z+)
			     (incf cie-z (* q (aref result)))))
			 finally
			 (let ((avg-x (* 1/rpp (the fast-float cie-x)))
			       (avg-y (* 1/rpp (the fast-float cie-y)))
			       (avg-z (* 1/rpp (the fast-float cie-z))))
			   (setf (aref array (+ (* 3 x) 0) y) avg-x)
			   (setf (aref array (+ (* 3 x) 1) y) avg-y)
			   (setf (aref array (+ (* 3 x) 2) y) avg-z)))))
       ;; Adaptive sampling.
       #+(or)
       (loop with error = 100.0
	     and spp1 = rays-per-pixel
	     and spp2 = (* 4 rays-per-pixel)
	     while (> error 0.01) do
       (setf error 0.0)
       (format stream "spp1 = ~A, spp2 = ~A~%" spp1 spp2)
       (loop for y from y0 below y1 do
	     (loop for x from x0 below x1 do
		   (loop with sampler2 = (stratified-sampler spp2)
		         with samples2 = (generate-samples
					  sampler2 spp2 6)
			 with cie-x of-type fast-float = 0.0
			 with cie-y of-type fast-float = 0.0
			 with cie-z of-type fast-float = 0.0
			 with 1/rpp of-type fast-float
			 = (/ 4.0 spp2)
			 repeat spp2 do
			 (let ((*depth* 0))
			   ;(print samples2 stream)
			   (eye-ray ray x y width height samples2)
			   (raytrace result
				     ray
				     objects
				     samples2
				     t)
			   (reset-pointer samples2)
			   (let ((q (aref result))
				 (nm (- (the fixnum (round (ray-nm ray))) 360)))
			     (incf cie-x (* q (aref +cie-1931-x+ nm)))
			     (incf cie-y (* q (aref +cie-1931-y+ nm)))
			     (incf cie-z (* q (aref +cie-1931-z+ nm)))))
			 finally
			 (let ((avg-x (* 1/rpp (the fast-float cie-x)))
			       (avg-y (* 1/rpp (the fast-float cie-y)))
			       (avg-z (* 1/rpp (the fast-float cie-z))))
			   (incf error
				 (let* ((prev-y (aref array (+ (* 3 x) 1) y))
					(foo (1- (/ avg-y prev-y))))
				   (* foo foo)))
			   #+(or)
			   (setf error
				 (max error
				      (abs (- (/ (aref array (+ (* 3 x) 1) y)
						 avg-y)
					      1.0))))
			   #|
			   (setf (aref array (+ (* 3 x) 0) y) avg-x)
			   (setf (aref array (+ (* 3 x) 1) y) avg-y)
			   (setf (aref array (+ (* 3 x) 2) y) avg-z)
			   |#
			   (setf (aref array (+ (* 3 x) 0) y)
				 (/ (+ (* spp1 (aref array (+ (* 3 x) 0) y))
				       (* spp2 avg-x))
				    (+ spp1 spp2)))
			   (setf (aref array (+ (* 3 x) 1) y)
				 (/ (+ (* spp1 (aref array (+ (* 3 x) 1) y))
				       (* spp2 avg-y))
				    (+ spp1 spp2)))
			   (setf (aref array (+ (* 3 x) 2) y)
				 (/ (+ (* spp1 (aref array (+ (* 3 x) 2) y))
				       (* spp2 avg-z))
				    (+ spp1 spp2)))))))
	(setf error (/ error (* (- x1 x0) (- y1 y0))))
	(format stream "max error: ~A~%" error)
	(incf spp1 spp2)
	(setf spp2 (* 4 spp2)))
	(sb-thread:with-mutex (output-mutex)
	  (queue-push (list x0 y0 x1 y1) output))
	(sb-thread:signal-semaphore output-sem)))))
			      :name (format nil "Worker ~D" n)))))
      (declare (ignorable stream))

      (unwind-protect
	   (loop with colors repeat works do
		(sb-thread:wait-on-semaphore output-sem)
		(sb-thread:with-mutex (output-mutex)
		  (setq colors (queue-pop output)))
		(let* ((x0 (pop colors))
		       (y0 (pop colors))
		       (x1 (pop colors))
		       (y1 (pop colors))
		       (stream (the stream (ltk:wish-stream *wish*))))
		  (declare (type image-index x0 y0 x1 y1))
		  (format stream "~A put { " (ltk::name *image*))
		  (loop for y from y0 below y1 do
		       (format stream "{ ")
		       (loop for x from x0 below x1 do
			    (let* ((cie-x (aref array (+ (* 3 x) 0) y))
				   (cie-y (aref array (+ (* 3 x) 1) y))
				   (cie-z (aref array (+ (* 3 x) 2) y))
				   (r (+ (*  3.240479 cie-x)
					 (* -1.537150 cie-y)
					 (* -0.498535 cie-z)))
				   (g (+ (* -0.969256 cie-x)
					 (*  1.875991 cie-y)
					 (*  0.041556 cie-z)))
				   (b (+ (*  0.056648 cie-x)
					 (* -0.204043 cie-y)
					 (*  1.057311 cie-z)))
				   #|
				   (r (+ (*  1.967 cie-x)
					 (* -0.548 cie-y)
					 (* -0.297 cie-z)))
				   (g (+ (* -0.955 cie-x)
					 (*  1.938 cie-y)
					 (* -0.027 cie-z)))
				   (b (+ (*  0.064 cie-x)
					 (* -0.130 cie-y)
					 (*  0.982 cie-z)))
				   |#)
			      (with-preserved-luminosity (r g b)
				(setf (values r g b)
				      (clamp-towards-grey r g b)))
			      #+(or)
			      (let ((w (min 0.0 r g b)))
				(when (minusp w)
				  (decf r w)
				  (decf g w)
				  (decf b w)
				  (let* ((y2 (+ (* 0.212671 r)
						(* 0.715160 g)
						(* 0.072169 b)))
					 (x (/ cie-y y2)))
				    (setf r (* x r))
				    (setf g (* x g))
				    (setf b (* x b)))))
			      (flet ((clamp (x)
				       (max
					0
					(min
					 255
					 (the fixnum
					   (round (* 255 (expt x (/ 2.2)))))))))
				(format stream "#~2,'0X~2,'0X~2,'0X "
					(clamp r) (clamp g) (clamp b)))))
		       (format stream "} "))
		  (format stream "} -to ~D ~D ~D ~D~%" x0 y0 x1 y1)
		  (finish-output stream)))
	(dolist (thread workers)
	  (when (sb-thread:thread-alive-p thread)
	    (sb-thread:terminate-thread thread)))))))

#|
 3.240479 -1.537150 -0.498535
-0.969256  1.875991  0.041556
 0.055648 -0.204043  1.057311

(let ((ntsc-r (+ (*  1.967 avg-x)
		 (* -0.548 avg-y)
		 (* -0.297 avg-z)))
      (ntsc-g (+ (* -0.955 avg-x)
		 (*  1.938 avg-y)
		 (* -0.027 avg-z)))
      (ntsc-b (+ (*  0.064 avg-x)
		 (* -0.130 avg-y)
		 (*  0.982 avg-z))))
  (flet ((clamp (x)
	   (max
	    0
	    (min
	     255
	     (the fixnum (round (* 255 x)))))))
    (return
      (list (clamp ntsc-r)
	    (clamp ntsc-g)
	    (clamp ntsc-b)))))
|#

(defun make-triangle-facing-away-from (p1 p2 p3 q1 material)
  (let ((triangle
	 (make-instance 'triangle :p1 p1 :p2 p2 :p3 p3 :material material)))
    (with-vec (nx ny nz) (normal triangle p1)
      (let ((rx (- (vec-x q1) (vec-x p1)))
	    (ry (- (vec-y q1) (vec-y p1)))
	    (rz (- (vec-z q1) (vec-z p1))))
	(if (minusp (+ (* nx rx) (* ny ry) (* nz rz)))
	    (make-instance 'triangle :p1 p2 :p2 p1 :p3 p3 :material material)
	    triangle)))))

(defun make-quadrangle-facing-away-from (p1 p2 p3 p4 q1 material)
  (list
   (make-triangle-facing-away-from p1 p2 p3 q1 material)
   (make-triangle-facing-away-from p3 p4 p1 q1 material)))

(defun make-quadrangle (p1 p2 p3 p4 material)
  (flet ((dist (p1 p2)
	   (with-vec (p1x p1y p1z) p1
	     (with-vec (p2x p2y p2z) p2
	       (let ((dx (- p1x p2x))
		     (dy (- p1y p2y))
		     (dz (- p1z p2z)))
		 (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))))))
    (print (list (dist p1 p3) (dist p2 p4)))
    (if (< (dist p1 p3) (dist p2 p4))
	(list
	 (make-instance 'triangle :p1 p1 :p2 p2 :p3 p3 :material material)
	 (make-instance 'triangle :p1 p3 :p2 p4 :p3 p1 :material material))
	(list
	 (make-instance 'triangle :p1 p1 :p2 p2 :p3 p4 :material material)
	 (make-instance 'triangle :p1 p2 :p2 p3 :p3 p4 :material material)))))

(defun make-prism (x1 x2 x3 y1 y2 y3 material)
  (append
   (list (make-triangle-facing-away-from x1 x2 x3 y1 material))
   (list (make-triangle-facing-away-from y1 y2 y3 x1 material))
   (make-quadrangle-facing-away-from x1 x2 y2 y1 x3 material)
   (make-quadrangle-facing-away-from x2 x3 y3 y2 x1 material)
   (make-quadrangle-facing-away-from x3 x1 y1 y3 x2 material)))

(defun sellmeier-glass (b1 b2 b3 c1 c2 c3)
  (lambda (nm)
    (let* ((x (* .001 nm))
	   (x2 (* x x)))
      (+ 1.0
	 (/ (* b1 x2) (- x2 c1))
	 (/ (* b2 x2) (- x2 c2))
	 (/ (* b3 x2) (- x2 c3))))))

(defun sellmeier-diamond ()
  (lambda (nm)
    (let* ((x (* .001 nm))
	   (x2 (* x x)))
      (sqrt (+ 1.0
	       (/ (* 4.3356 x2) (- x2 0.011236))
	       (/ (* 0.3306 x2) (- x2 0.030625)))))))

(let ((table (make-hash-table :test 'equal)))
  (defun cauchy (a b)
    (setf a (coerce a 'fast-float))
    (setf b (coerce b 'fast-float))
    (or (gethash (list a b) table)
	(setf (gethash (list a b) table)
	      (lambda (nm)
		(setf nm (* .001 nm))
		(+ a (/ b (* nm nm))))))))

(defconstant +vacuum+  (cauchy 1.0 0.0))
(defconstant +water+   (cauchy 1.3   .00531))
(defconstant +glass+   (cauchy 1.569 .00531))
(defconstant +glass2+  (cauchy 1.7   .00531))
(defconstant +diamond+ (sellmeier-diamond))

(defun make-color (amplitude from-nm to-nm)
  (lambda (nm)
    (if (<= from-nm nm to-nm)
	amplitude
	0.0)))

(defparameter *objects*
  #+(or)
  (let ((list nil)
	(material (make-material :specular 1.0))
	(d .5))
    (loop for u from 0 to 2pi by d do
	  (loop for v from 0 to pi by d do
		(let ((u2 (+ u d))
		      (v2 (+ v d)))
		  (push (make-instance 'triangle
				       :p1 (vec (* 250.0 (cos v) (cos u))
						(* 250.0 (sin u))
						(* 250.0 (sin v) (cos u)))
				       :p2 (vec (* 250.0 (cos v) (cos u2))
						(* 250.0 (sin u2))
						(* 250.0 (sin v) (cos u2)))
				       :p3 (vec (* 250.0 (cos v2) (cos u))
						(* 250.0 (sin u))
						(* 250.0 (sin v2) (cos u)))
				       :material material)
			list)
		  (push (make-instance 'triangle
				       :p1 (vec (* 250.0 (cos v) (cos u2))
						(* 250.0 (sin u2))
						(* 250.0 (sin v) (cos u2)))
				       :p2 (vec (* 250.0 (cos v2) (cos u))
						(* 250.0 (sin u))
						(* 250.0 (sin v2) (cos u)))
				       :p3 (vec (* 250.0 (cos v2) (cos u2))
						(* 250.0 (sin u2))
						(* 250.0 (sin v2) (cos u2)))
				       :material material)
			list))))
    list)
  #+(or)
  (list*
   (make-instance 'sphere
		  :pos (vec 300 90 200) ; (300 150 300)
		  :radius 120.0
		  :material (make-material :specular 1.0
					   #+(or)#+(or)
					   :bump (roughness .05)))
   (make-instance 'sphere
		  :pos (vec -150 60 -60)
		  :radius 150.0
		  :material (make-material :diffuse .7)
		            #+(or)
		            (make-material
			     :diffuse
			     (color-spike .7 500.0)))
			       
   (make-instance 'sphere
		  :pos #+(or)(vec 70 -50 -450) (vec 30 -50 -450)
		  :radius 40.0
		  :material (make-material :diffuse 1.0))
   (make-instance 'triangle
		  :p1 (vec -100   0 -400)
		  :p2 (vec -100   0 -600)
		  :p3 (vec -100 -85 -600)
		  :material (make-material :specular 1.0))
   (make-instance 'triangle
		  :p1 (vec -100   0 -400)
		  :p2 (vec -100 -85 -400)
		  :p3 (vec -100 -85 -600)
		  :material (make-material :specular 1.0))
   (make-instance 'triangle
		  :p1 (vec -102   0 -400)
		  :p2 (vec -102   0 -600)
		  :p3 (vec -102 -85 -600)
		  :material (make-material :diffuse .1))
   (make-instance 'triangle
		  :p1 (vec -102   0 -400)
		  :p2 (vec -102 -85 -400)
		  :p3 (vec -102 -85 -600)
		  :material (make-material :diffuse .1))
   #+(or)
   (make-instance 'pipe
		  :p1 (vec -300 300 -200)
		  :p2 (vec -300 300 1000)
		  :r1 20.0
		  :r2 20.0
		  :material (make-material :emit 2.0))
   #+(or)
   (make-instance 'sphere
		  :pos (vec -300 200 0)
		  :radius 30.0
		  :material (make-material :emit 3.0))
   #+(or)
   (make-instance 'sphere
		  :pos (vec 50 60 0)
		  :radius 150.0
		  :material (make-material :transmission 1.0
					   :n1 1.0 :n2 1.5))
   #+(or)
   (make-instance 'sphere
		  :pos (vec 50 60 0)
		  :radius 145.0
		  :material (make-material :transmission 1.0
					   :n1 1.5 :n2 1.0))
   #+(or)
   (make-instance 'ring
		  :pos (vec 0 0 3000)
		  :normal (normalize (vec 1 0 -1))
		  :rmin2 0.0
		  :rmax2 (* 3000.0 3000.0)
		  :material
		  (let ((m (make-material)))
		    (lambda (p)
		      (with-vec (x y z) p
			(let ((x (round x 5.0))
			      (z (round y 5.0)))
			  (if (or (zerop (logand 15 x))
				  (zerop (logand 15 z)))
			      (setf (diffuse m) .3)
			      (setf (diffuse m) .9))
			  m)))))
   (make-instance 'ring
		  :pos (vec 0 -90 0)
		  :normal (normalize (vec 0 1 0))
		  :rmin2 0.0
		  :rmax2 (* 10000.0 10000.0)
		  :material
		  (let ((m (make-material)))
		    (lambda (p)
		      (with-vec (x y z) p
			(let ((x (round (+ x (* .3 z)) 5.0))
			      (z (round (- z (* .3 x)) 5.0)))
			  (if (or (zerop (logand 15 x))
				  (zerop (logand 15 z)))
			      (setf (diffuse m) .3)
			      (setf (diffuse m) .9))
			  m)
			#+(or)
			(let ((x (round (+ x (* .3 z)) 80.0))
			      (z (round (- z (* .3 x)) 80.0)))
			  (if (zerop (logand 1 (logxor x z)))
			      (setf (diffuse m) .7)
			      (setf (diffuse m) .9))
			  m)))))
   nil
   #+(or)
   (loop for x from -400 to 1000 by 20
	 collect (make-instance 'sphere
				:pos (vec x -75 (* 3 x))
				:radius 15.0
				:material
				(make-material
				 :specular 0.5
				 :bump (roughness .2))))
   #+(or)
   (loop for u from 0 to 2pi by 0.5
	 append (let* ((v (+ u 0.5))
		       (x1 (* 100.0 (cos u)))
		       (y1 (- (* u 30) 50))
		       (z1 (* 100.0 (sin u)))
		       (x2 (* 100.0 (cos v)))
		       (y2 (- (* v 30) 50))
		       (z2 (* 100.0 (sin v))))
		  (list
		   (make-instance 'sphere
				  :pos (vec x1 y1 z1)
				  :radius 20.0
				  :material (make-material :specular 1.0))
		   (make-instance 'pipe
				  :p1 (vec x1 y1 z1)
				  :p2 (vec x2 y2 z2)
				  :r1 20.0
				  :r2 20.0
				  :material (make-material :specular 1.0))))))
  #+(or)
  (list*
   ;; Light.
   (make-instance 'ring
		    :pos (vec -1000 1000 500)
		    :normal (normalize (vec 1 -1 -.5))
		    :rmin2 0.0
		    :rmax2 (* 200.0 200.0)
		    :material (make-material :emit 1.0))
   ;;Floor.
   #+(or)
   (make-instance 'ring
		  :pos (vec 0 -140 0)
		  :normal (normalize (vec 0 1 0))
		  :rmin2 0.0
		  :rmax2 (* 10000.0 10000.0)
		  :material
		  (let ((m (make-material)))
		    (lambda (p)
		      (with-vec (x y z) p
			(declare (ignorable y))
			(let ((x (round (+ x (* .3 z)) 10.0))
			      (z (round (- z (* .3 x)) 10.0)))
			  (if (or (zerop (logand 15 x))
				  (zerop (logand 15 z)))
			      (setf (diffuse m) .3)
			      (setf (diffuse m) .9))
			  m)))))
   ;;Teapot.
   ;;#+(or)
   (loop for patch in (subseq teapot::*teapot-patches* 4 6)
	 with d = 1/8 nconc
	 (loop for u from 0 to .99999 by d nconc
	       (loop for v from 0 to .99999 by d
		     for (x1 y1 z1) = (teapot::q patch u v)
		     for (x2 y2 z2) = (teapot::q patch (+ u d) v)
		     for (x3 y3 z3) = (teapot::q patch (+ u d) (+ v d))
		     for (x4 y4 z4) = (teapot::q patch u (+ v d))
		     nconc (make-quadrangle
			    (vec (* 100 x1) (- (* 100 z1) 140) (* 100 y1))
			    (vec (* 100 x2) (- (* 100 z2) 140) (* 100 y2))
			    (vec (* 100 x3) (- (* 100 z3) 140) (* 100 y3))
			    (vec (* 100 x4) (- (* 100 z4) 140) (* 100 y4))
			    (make-material :diffuse 1.0))))))
  #+(or)
  `(,@(make-quadrangle-facing-away-from
       (vec  -800 1200 300)
       (vec  -800 1200 700)
       (vec -1200  800 700)
       (vec -1200  800 300)
       (vec 0 0 0)
       (make-material :emit 3.0))
    #+(or)
    ,(make-instance 'ring
		    :pos (vec -1000 1000 500)
		    :normal (normalize (vec 1 -1 -.5))
		    :rmin2 0.0
		    :rmax2 (* 200.0 200.0)
		    :material (make-material :emit 2.0))
    ,(make-instance 'sphere
		    :pos (vec -100 60 0)
		    :radius 200.0
		    :material (make-material :transmission 1.0
					     :n1 +vacuum+ :n2 +glass+))
    ,(make-instance 'sphere
		    :pos (vec 300 150 100)
		    :radius 100.0
		    :material (make-material :specular 1.0))
    ,(make-instance 'sphere
		    :pos (vec 160 -110 -150)
		    :radius 30.0
		    :material (make-material
			       :diffuse (make-color 1.0 500 600)))
    #+(or)
    ,@(make-quadrangle-facing-away-from
       (vec -50 -90 -500)
       (vec -80 -90 -490)
       (vec -80  50 -490)
       (vec -50  50 -500)
       (vec -80   0 -600)
       (make-material :transmission 1.0 :n1 +vacuum+ :n2 +glass+))
    #+(or)
    ,@(make-prism (vec  50 -89 -500)
		  (vec  20 -89 -490)
		  (vec -90 -89 -600)
		  (vec  50  50 -500)
		  (vec  20  50 -490)
		  (vec -90  50 -600)
		  (make-material :transmission 1.0
				 :n1 +vacuum+ :n2 +glass+))
    #+(or)
    ,(make-instance 'sphere
		    :pos (vec 0 30 -400)
		    :radius 110.0
		    :material (make-material :transmission 1.0
					     :n1 +glass+ :n2 +vacuum+))
    #+(or)
    ,(make-instance 'triangle
		    :p1 (vec -50 -90 -500)
		    :p2 (vec -50  50 -500)
		    :p3 (vec -80 -90 -490)
		    :material (make-material :transmission 1.0
					     :n1 +vacuum+ :n2 +glass+))
    #+(or)
    ,(make-instance 'triangle
		    :p1 (vec -50  50 -500)
		    :p2 (vec -80  50 -490)
		    :p3 (vec -80 -90 -490)
		    :material (make-material :transmission 1.0
					     :n1 +vacuum+ :n2 +glass+))
    #+(or)
    ,(make-instance 'triangle
		    :p1 (vec  -80  50 -490)
		    :p2 (vec -150  50 -550)
		    :p3 (vec -150 -90 -550)
		    :material (make-material :transmission 1.0
					     :n1 +vacuum+ :n2 +glass+))
    #+(or)
    ,(make-instance 'triangle
		    :p1 (vec  -80  50 -490)
		    :p2 (vec  -80 -90 -490)
		    :p3 (vec -150 -90 -550)
		    :material (make-material :transmission 1.0
					     :n1 +vacuum+ :n2 +glass+))
    #+(or)
    ,(make-instance 'triangle
		    :p1 (vec  -50  50 -500)
		    :p2 (vec -150  50 -550)
		    :p3 (vec -150 -90 -550)
		    :material (make-material :transmission 1.0
					     :n1 +vacuum+ :n2 +glass+))
    #+(or)
    ,(make-instance 'triangle
		    :p1 (vec  -50  50 -500)
		    :p2 (vec  -50 -90 -500)
		    :p3 (vec -150 -90 -550)
		    :material (make-material :transmission 1.0
					     :n1 +vacuum+ :n2 +glass+))
    ,(make-instance 'ring
		    :pos (vec 0 -140 0)
		    :normal (normalize (vec 0 1 0))
		    :rmin2 0.0
		    :rmax2 (* 10000.0 10000.0)
		    :material
		    (let ((m (make-material)))
		      (lambda (p)
			(with-vec (x y z) p
			  (declare (ignorable y))
			  (let ((x (round (+ x (* .3 z)) 10.0))
				(z (round (- z (* .3 x)) 10.0)))
			    (if (or (zerop (logand 15 x))
				    (zerop (logand 15 z)))
				(setf (diffuse m) .3)
				(setf (diffuse m) .9))
			    m)
			  #+(or)
			  (let ((x (round (+ x (* .3 z)) 150.0))
				(z (round (- z (* .3 x)) 150.0)))
			    (if (zerop (logand 1 (logxor x z)))
				(setf (diffuse m) .7)
				(setf (diffuse m) .9))
			    m))))))
  #+(or)
  (list
   ;; light
   #+(or)
   (make-instance 'sphere
		  :pos (vec 0 200 0)
		  :radius 99.0
		  :material (make-material :emit 3.0))
   (make-instance 'ring
		  :pos (vec 0 400 0)
		  :normal (vec 0 -1 0)
		  :rmin2 0.0
		  :rmax2 (* 100.0 100.0)
		  :material (make-material :emit 20.0))
   ;; diffuse ball
   (make-instance 'sphere
		  :pos (vec -100 0 20)
		  :radius 100.0
		  :material (make-material :diffuse 1.0))
   ;; mirror ball
   (make-instance 'sphere
		  :pos (vec 300 50 200)
		  :radius 150.0
		  :material (make-material :specular .8))
   ;; ceiling
   (make-instance 'ring
		  :pos (vec 0 200 0)
		  :normal (vec 0 -1 0)
		  :rmin2 (*  70.0  70.0)
		  :rmax2 (* 500.0 500.0)
		  :material (make-material :diffuse .9))
   ;; floor
   (make-instance 'triangle
		  :p1 (vec -300 -200  200)
		  :p2 (vec  300 -200  200)
		  :p3 (vec  300 -200 -400)
		  :material (make-material :diffuse .9))
   (make-instance 'triangle
		  :p1 (vec -300 -200  200)
		  :p2 (vec -300 -200 -400)
		  :p3 (vec  300 -200 -400)
		  :material (make-material :diffuse .9))
   #+(or)
   (make-instance 'ring
		  :pos (vec 0 -200 0)
		  :normal (vec 0 1 0)
		  :rmin2 0.0
		  :rmax2 (* 1000.0 1000.0)
		  :material (make-material :diffuse .9))
   ;; back wall
   (make-instance 'triangle
		  :p1 (vec -300 -200 200)
		  :p2 (vec  300 -200 200)
		  :p3 (vec  300  200 200)
		  :material (make-material :diffuse .9))
   (make-instance 'triangle
		  :p1 (vec -300 -200 200)
		  :p2 (vec -300  200 200)
		  :p3 (vec  300  200 200)
		  :material (make-material :diffuse .9))
   #+(or)
   (make-instance 'ring
		  :pos (vec 0 0 200)
		  :normal (vec 0 0 -1)
		  :rmin2 0.0
		  :rmax2 (* 350.0 350.0)
		  :material (make-material :diffuse .9))
   ;; left wall
   (make-instance 'triangle
		  :p1 (vec -300  200  200)
		  :p2 (vec -300 -200  200)
		  :p3 (vec -300 -200 -400)
		  :material (make-material :diffuse .9))
   (make-instance 'triangle
		  :p1 (vec -300  200  200)
		  :p2 (vec -300  200 -400)
		  :p3 (vec -300 -200 -400)
		  :material (make-material :diffuse .9))
   #+(or)
   (make-instance 'ring
		  :pos (vec -300 0 0)
		  :normal (vec 1 0 0)
		  :rmin2 0.0
		  :rmax2 (* 350.0 350.0)
		  :material (make-material :diffuse .9))
   ;; right wall
   (make-instance 'triangle
		  :p1 (vec 300  200  200)
		  :p2 (vec 300 -200  200)
		  :p3 (vec 300 -200 -400)
		  :material (make-material :diffuse .9))
   (make-instance 'triangle
		  :p1 (vec 300  200  200)
		  :p2 (vec 300  200 -400)
		  :p3 (vec 300 -200 -400)
		  :material (make-material :diffuse .9))
   #+(or)
   (make-instance 'ring
		  :pos (vec 300 0 0)
		  :normal (vec -1 0 0)
		  :rmin2 0.0
		  :rmax2 (* 350.0 350.0)
		  :material (make-material :diffuse .9)))
  #+(or)
  (list
   (make-instance 'sphere
		  :pos (vec -380 0 0)
		  :radius 280.0
		  :material (make-material :diffuse 1.0))
   (make-instance 'ring
		  :pos (vec 350 0 0)
		  :normal (normalize (vec -1 0 0))
		  :rmin2 0.0
		  :rmax2 (* 100.0 100.0)
		  :material (make-material :emit 20.0))
   ;;#+(or)
   (make-instance 'sphere
		  :pos (vec -30 -50 -100)
		  :radius 30.0
		  :material (make-material :diffuse 1.0)))
  ;;#+(or)
  (list
   (make-instance 'ring
		    :pos (vec -1000 1000 500)
		    :normal (normalize (vec 1 -1 -.5))
		    :rmin2 0.0
		    :rmax2 (* 200.0 200.0)
		    :material (make-material :emit 1.0))
   (make-instance 'sphere
		  :pos '(#.(+ 1e5 1) 40.8 81.6)
		  :radius 1e5
		  :material (make-material :diffuse .5)) ;(.75,.25,.25)
   (make-instance 'sphere :pos '(#.(+ -1e5 99) 40.8b81.6)
		  :radius 1e5
		  :material (make-material :diffuse .5)) ;(.25,.25,.75)
   (make-instance 'sphere :pos '(50 40.8 1e5)
		  :radius 1e5
		  :material (make-material :diffuse .75))
   (make-instance 'sphere :pos '(50 40.8 #.(+ -1e5 170))
		  :radius 1e5
		  :material (make-material :diffuse 0.0))
   (make-instance 'sphere :pos '(50 1e5 81.6)
		  :radius 1e5
		  :material (make-material :diffuse .75))
   (make-instance 'sphere :pos '(50 #.(+ -1e5 81.6) 81.6)
		  :radius 1e5
		  :material (make-material :diffuse .75))
   (make-instance 'sphere :pos '(27 16.5 47)
		  :radius 16.5
		  :material (make-material :specular .999))
   (make-instance 'sphere :pos '(73 16.5 88)
		  :radius 16.5
		  :material (make-material :transmission .999
					   :n1 1.0 :n2 1.5))
   (make-instance 'sphere :pos '(50 8.5 60)
		  :radius 8.5
		  :material (make-material :diffuse .999))
   (make-instance 'sphere
		  :pos '(50 8.5 60)
		  :radius 8.5
		  :material (make-material :diffuse .999))
   (make-instance 'sphere
		  :pos '(50 #.(- 681.6 .27) 81.6)
		  :radius 600.0
		  :material (make-material :emit 12.0))))

(defun how-much-time (seconds)
  (multiple-value-bind (seconds minutes hours days months)
      (decode-universal-time (round seconds))
    (decf months)
    (decf days)
    (decf hours)
    (with-output-to-string (s)
      (when (plusp months)
	(format s "~a months, " months))
      (when (plusp days)
	(format s "~a days, " days))
      (when (plusp hours)
	(format s "~a hours, " hours))
      (when (plusp minutes)
	(format s "~a minutes, " minutes))
      (format s "~a seconds" seconds))))

(defun mane (&key (width 800) (height 600) (rays 16))
  (with-window (:width width :height height)
    (let* ((array (make-array (list (* 3 width) height)
			      :element-type 'fast-float
			      :initial-element 0.0))
	   (objects *objects*)
	   (tree objects) ;(make-kd-tree objects :max-per-node 10))
	   (*rays* 0)
	   (*primary-rays* 0)
	   (*shadow-rays* 0)
	   (start-time (get-internal-run-time)))
      (render array tree :rays-per-pixel rays)
      (let* ((end-time (get-internal-run-time))
	     (seconds (/ (- end-time start-time)
			 internal-time-units-per-second))
	     (description (format
			   nil
			   "time: ~,2F seconds, ~
                            primary rays: ~A, ~
                            shadow rays: ~A, ~
                            rays: ~A"
			   seconds *primary-rays* *shadow-rays* *rays*)))
	;;#+(or)
	(hdri-io:write "/tmp/tmp.exr" array :description description)
	;;#+(or)
	(hdri-io:write "/tmp/tmp.hdr" array :description description)
	;;#+(or)
	(hdri-io:write "/tmp/tmp.pfs" array :description description)
	;;#+(or)
	(hdri-io:write "/tmp/tmp.pfm" array :description description)
	(format t "~&Time:                     ~,2F seconds (~A).~@
                     Primary rays:             ~A.~@
                     Shadow rays:              ~A.~@
                     Rays:                     ~A.~@
                     Primary rays per pixel:   ~,1F.~@
                     Rays per pixel:           ~,2F.~@
                     Rays per second:          ~,0F~%"
		seconds	(how-much-time seconds) *primary-rays*
		*shadow-rays* *rays* (/ *primary-rays* width height)
		(/ *rays* width height) (/ *rays* seconds)))
      nil)))

(defun raytrace-ppm (result ray objects samples photon-pass-p)
  (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (declare (box result))
  (let ((*depth* (1+/fixnum *depth*)))
    (when (> *depth* 100)
      (format t "~&Deep path.~%")
      (setf (aref result) 0.0)
      (return-from raytrace-ppm nil))
    (let* ((hit (vec 0 0 0))
	   (object (kd-intersect objects ray hit))
	   (path-sample (get-next-sample samples)))
      (declare (vec hit))
      (declare (dynamic-extent hit))
      (if object
	  (let* ((m1 (material object))
		 (m (if (functionp m1) (funcall m1 hit) m1))
		 (diffuse (diffuse m))
		 (diffuse (if (functionp diffuse)
			      (funcall diffuse (ray-nm ray))
			      diffuse))
		 (specular (specular m))
		 (transmission (transmission m))
		 (r (random 1.0))
		 (n (normal object hit))
		 (transmitted-ray nil))
	    (declare (fast-float diffuse specular transmission r))

	    #+(or)
	    (when (plusp transmission)
	      (with-vec (dx dy dz) (ray-dir ray)
		(let ((hit2 (vec (+ (vec-x hit) (* .1 dx))
				 (+ (vec-y hit) (* .1 dy))
				 (+ (vec-z hit) (* .1 dz))))
		      (n1 (funcall (the function (n1 m)) (ray-nm ray)))
		      (n2 (funcall (the function (n2 m)) (ray-nm ray))))
		  (setf transmitted-ray
			(refraction ray hit2 (normal object hit) n1 n2))
		  (if transmitted-ray
		      (with-vec (nx ny nz) n
			(with-vec (tx ty tz) (ray-dir transmitted-ray)
			  (with-vec (dx dy dz) (ray-dir ray)
			    (let ((r (* transmission 
					(dieletric-fresnel
					 (- (+ (* nx dx)
					       (* ny dy)
					       (* nz dz)))
					 (abs
					  (+ (* nx tx)
					     (* ny ty)
					     (* nz tz)))
					 n1 n2))))
			      (setf specular (+ specular r)
				    transmission (- transmission r))))))
		      ;; Total reflection.
		      (setf specular (+ specular transmission)
			    transmission 0.0)))))

	    (cond
	      ((< r (if (<= *depth* 3)
			(if (plusp diffuse) 1.0 0.0)
			diffuse))
	       (if photon-pass-p
		   nil ;hash, update hitpoint
		   nil ;(push (make-hitpoint hit normal ...) hitpoints)
		   ))
	      ((< diffuse r (+ diffuse specular))
	       (raytrace-ppm result (reflection ray hit n (bump m))
			     objects samples photon-pass-p))
	      ((< (+ diffuse specular) r (+ diffuse specular transmission))
	       (raytrace-ppm result transmitted-ray objects samples photon-pass-p))
	      (t
	       (setf (aref result) 0.0))))
	  (setf (aref result) 0.0))))
  nil)

(defun ppm (&key (width 400) (height 300) (rays 4) (photons 1000))
  (with-window (:width width :height height)
    (let* ((array (make-array (list (* 3 width) height)
			      :element-type 'fast-float
			      :initial-element 0.0))
	   (objects *objects*)
	   (tree (make-kd-tree objects :max-per-node 10))
	   (*rays* 0)
	   (*primary-rays* 0)
	   (*shadow-rays* 0)
	   (sampler (stratified-sampler rays))
	   (start-time (get-internal-run-time)))
      (loop for y from 0 below height do
	   (loop for x from 0 below width do
		(loop with samples = (generate-samples
				      sampler rays 6)
			 with cie-x of-type fast-float = 0.0
			 with cie-y of-type fast-float = 0.0
			 with cie-z of-type fast-float = 0.0
			 repeat 1 do
			 (let ((*depth* 0)
			       (result (make-box))
			       (ray (make-ray)))
			   (eye-ray ray x y width height samples)
			   (raytrace-ppm result
					 ray
					 tree
					 samples
					 t)
			   (reset-pointer samples)
			   (let ((q (* (aref result) (ray-weight ray)))
				 (nm (ray-nm ray)))
			     (%wavelength-response result nm +cie-1931-x+)
			     (incf cie-x (* q (aref result)))
			     (%wavelength-response result nm +cie-1931-y+)
			     (incf cie-y (* q (aref result)))
			     (%wavelength-response result nm +cie-1931-z+)
			     (incf cie-z (* q (aref result)))))
			 finally
			   (setf (aref array (+ (* 3 x) 0) y) cie-x)
			   (setf (aref array (+ (* 3 x) 1) y) cie-y)
			   (setf (aref array (+ (* 3 x) 2) y) cie-z))))      

      (let* ((end-time (get-internal-run-time))
	     (seconds (/ (- end-time start-time)
			 internal-time-units-per-second))
	     (description (format
			   nil
			   "time: ~,2F seconds, ~
                            primary rays: ~A, ~
                            shadow rays: ~A, ~
                            rays: ~A"
			   seconds *primary-rays* *shadow-rays* *rays*)))
	;;#+(or)
	(hdri-io:write "/tmp/tmp.exr" array :description description)
	;;#+(or)
	(hdri-io:write "/tmp/tmp.hdr" array :description description)
	;;#+(or)
	(hdri-io:write "/tmp/tmp.pfs" array :description description)
	;;#+(or)
	(hdri-io:write "/tmp/tmp.pfm" array :description description)
	(format t "~&Time:                     ~,2F seconds (~A).~@
                     Primary rays:             ~A.~@
                     Shadow rays:              ~A.~@
                     Rays:                     ~A.~@
                     Primary rays per pixel:   ~,1F.~@
                     Rays per pixel:           ~,2F.~@
                     Rays per second:          ~,0F~%"
		seconds	(how-much-time seconds) *primary-rays*
		*shadow-rays* *rays* (/ *primary-rays* width height)
		(/ *rays* width height) (/ *rays* seconds)))
      nil)))  

(defun color-spike (amplitude base-nm)
  (lambda (nm)
    (let ((x (- nm base-nm)))
      (* amplitude (exp (* -.0002 x x))))))

(defun rainbow (image x y fn)
  (flet ((integral (array)
	   (loop for nm from 360 to 830
		 sum (* (funcall fn nm)
			(aref array (round (- nm 360)))))))
    (let ((cie-x (integral +cie-1931-x+))
	  (cie-y (integral +cie-1931-y+))
	  (cie-z (integral +cie-1931-z+)))
      (let ((ntsc-r (+ (*  1.967 cie-x) (* -0.548 cie-y) (* -0.297 cie-z)))
	    (ntsc-g (+ (* -0.955 cie-x) (*  1.938 cie-y) (* -0.027 cie-z)))
	    (ntsc-b (+ (*  0.064 cie-x) (* -0.130 cie-y) (*  0.982 cie-z))))
	(let ((colors (list (max (min (round ntsc-r) 255) 0)
			    (max (min (round ntsc-g) 255) 0)
			    (max (min (round ntsc-b) 255) 0))))
	  (image-setpixel image (list (list colors)) x y (1+ x) (1+ y)))))))


;;#+(or)
(progn
  (deftype kd-tree ()		'(simple-array t (4)))
  (defun make-tree (&rest stuff) (make-array 4 :initial-contents stuff))
  (defmacro tree-axis (tree)	`(the function (aref (the kd-tree ,tree) 0)))
  (defmacro tree-split (tree)	`(the fast-float (aref (the kd-tree ,tree) 1)))
  (defmacro tree-left (tree)	`(aref (the kd-tree ,tree) 2))
  (defmacro tree-right (tree)	`(aref (the kd-tree ,tree) 3)))

#+(or)
(progn
  (defstruct (kd-tree
	       (:constructor make-tree (axis split left right))
	       (:conc-name tree-))
    (axis nil :type function)
    (split 0.0 :type fast-float)
    (left nil :type (or list kd-tree))
    (right nil :type (or list kd-tree))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun x-axis (vec) (vec-x vec))
  (defun y-axis (vec) (vec-y vec))
  (defun z-axis (vec) (vec-z vec)))

(defun make-kd-tree (objects &key (max-per-node 10) (max-depth 10))
  (declare (fixnum max-per-node))
  (labels ((plane-axis (plane)
	     (ecase (car plane)
	       (:x #'x-axis)
	       (:y #'y-axis)
	       (:z #'z-axis)))
	   (plane-split (plane)
	     (cdr plane))
	   (subdivide (objects depth min max)
	     (if (or (<= (length (the list objects)) max-per-node)
		     (zerop depth))
		 objects
		 (let ((plane (compute-plane objects min max)))
		   (destructuring-bind (lmin lmax rmin rmax)
		       (split-box min max plane)
		     (let ((left (partition objects plane #'first #'<))
			   (right (partition objects plane #'second #'>)))
		       (print (list :plane plane
				    :parent (length objects)
				    :left (length left)
				    :right (length right)))
		       (if (or (= (length objects) (length left))
			       (= (length objects) (length right)))
			   objects
			   (make-tree (plane-axis plane)
				      (plane-split plane)
				      (subdivide left (1- depth) lmin lmax)
				      (subdivide right (1- depth) rmin rmax)))
		       #+(or)
		       (make-instance 'kd-tree
				      :plane plane
				      :left (subdivide left lmin lmax)
				      :right (subdivide right rmin rmax)))))))
	   (find-split (objects dim min max)
	     (loop with split = .5
		for x = .25 then (/ x 2)
		for plane = (cons dim (+ min (* (- max min) split)))
		for p1 = (partition objects plane #'first #'<)
		for p2 = (partition objects plane #'second #'>)
		do (print (list split (length p1) (length p2)))
		  (cond
		    ((< x .00001)
		     (return plane))
		    ((or (null p2) (> (/ (length p1) (length p2)) 1.25))
		     (decf split x))
		    ((or (null p1) (> (/ (length p2) (length p1)) 1.25))
		     (incf split x))
		    (t
		     (return plane)))))
	   (compute-plane (objects min max)
	     (with-vec (minx miny minz) min
	       (with-vec (maxx maxy maxz) max
		 (let ((dx (- maxx minx))
		       (dy (- maxy miny))
		       (dz (- maxz minz)))
		   (print (list :dx dx :dy dy :dz dz))
		   (cond
		     ((and (> dx dy) (> dx dz))
		      (find-split objects :x minx maxx)
		      #+(or)
		      (cons :x (* .5 (+ maxx minx))))
		     ((> dy dz)
		      (find-split objects :y miny maxy)
		      #+(or)
		      (cons :y (* .5 (+ maxy miny))))
		     (t
		      (find-split objects :z minz maxz)))))))
	   (split-box (min max plane)
	     (with-vec (minx miny minz) min
	       (with-vec (maxx maxy maxz) max
		 (let ((q (cdr plane)))
		   (ecase (car plane)
		     (:x (list (vec minx miny minz)
			       (vec q    maxy maxz)
			       (vec q    miny minz)
			       (vec maxx maxy maxz)))
		     (:y (list (vec minx miny minz)
			       (vec maxx q    maxz)
			       (vec minx q    minz)
			       (vec maxx maxy maxz)))
		     (:z (list (vec minx miny minz)
			       (vec maxx maxy q)
			       (vec minx miny q)
			       (vec maxx maxy maxz))))))))
	   (partition (objects plane min-or-max compare)
	     (let ((q (cdr plane)))
	       (remove-if-not
		(lambda (object)
		  (with-vec (x y z) (funcall min-or-max (bounding-box object))
		    (ecase (car plane)
		      (:x (funcall compare x q))
		      (:y (funcall compare y q))
		      (:z (funcall compare z q)))))
		objects))))
    (apply #'subdivide objects max-depth (bounding-box objects))))

(defmacro defun/typed (name lambda-list return-type &body body)
  (let ((arg-types (loop for arg in lambda-list
			 if (consp arg)
			   collect (second arg)
			 else
			   collect t))
	 (arg-names (loop for arg in lambda-list
			  if (consp arg)
			    collect (first arg)
			  else
			    collect arg)))
    `(progn
       (declaim (ftype (function ,arg-types (value ,return-type)) ,name))
       (defun ,name ,arg-names ,@body))))

(defmacro axis (tree vec)
  `(the fast-float (funcall (tree-axis ,tree) ,vec)))

(defun kd-print (tree &optional (depth 0))
  (write-string (make-array depth :element-type 'character
			    :initial-element #\Space))
  (incf depth)
  (cond
    ((listp tree)
     (format t "(~D objects)~%" (length tree)))
    (t
     (format t "~A ~,3F~%" (tree-axis tree) (tree-split tree))
     (kd-print (tree-left tree) depth)
     (kd-print (tree-right tree) depth))))

#|
(defun kd-draw (tree box)
  (with-window (:width 800 :height 600)
    (flet ((draw ()
	     (with-vec (minx miny minz) (first box)
	       (with-vec (maxx maxy maxz) (second box)
		 (ecase (tree-axis tree)
		   (#.#'x-axis
		    (draw-line (tree-split tree) miny (tree-split tree) maxy))
		   (#.#'y-axis
		    (draw-line minx (tree-split tree) maxx (tree-split tree)))
		   (#.#'z-axis
		    nil))
		 (draw (tree-left tree))
		 (draw (tree-right tree))))))
      (draw))))
|#

(defun kd-intersect (tree ray hit)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;;(format t "~&tree = ~A~&ray = ~A" tree ray)
  (macrolet ((node-intersect (near far cmp)
	       (let ((obj (gensym)))
		 `(let ((,obj (kd-intersect (,near tree) ray hit)))
		    (cond
		      ((and ,obj (,cmp (axis tree hit) (tree-split tree)))
		       ,obj)
		      ((,cmp 0.0 (axis tree (ray-dir ray)))
		       (kd-intersect (,far tree) ray hit))
		      (t
		       nil))))))
    (cond
      ((listp tree)
       (list-intersect tree ray hit))
      ((<= (axis tree (ray-pos ray)) (tree-split tree))
       (node-intersect tree-left tree-right <=))
      (t
       (node-intersect tree-right tree-left >=)

       #+(or)
       (let ((object (kd-intersect (tree-right tree) ray hit)))
	 (if (and object (>= (axis tree hit) (tree-split tree)))
	     object
	     (if (minusp (axis tree (ray-dir ray)))
		 (kd-intersect (tree-left tree) ray hit)
		 nil)))))))

#+(or)
(defmacro defun/cc (name (x) &body body)
  (let* ((num (random #x100000))
	 (c-file-name (format nil "/tmp/file~X.c" num))
	 (o-file-name (format nil "/tmp/file~X.o" num))
	 (so-file-name (format nil "/tmp/file~X.so" num))
	 (c-function-name (format nil "foo~X" num)))
    `(progn
       (with-open-file (s ,c-file-name
			:direction :output
			:if-exists :supersede)
	 (format s "int ~A (int ~A) { return ~{~A~}; }"
	         ,c-function-name ',x ',body))
       (sb-ext:run-program
	"gcc" '("-fPIC" "-c" ,c-file-name "-o" ,o-file-name) :search t)
       (sb-ext:run-program
	"gcc" '("-shared" "-o" ,so-file-name ,o-file-name) :search t)
       (cffi:load-foreign-library ,so-file-name)
       (cffi:defcfun (,c-function-name ,name) :int (x :int)))))

#+(or)
(defun/cc foo (x)
  x + 42)

#|
list => 67.30 seconds.
10 => 68.64 seconds.	46.64 seconds.
15 => 49.50 seconds.	33.76 seconds.
20 => 50.21 seconds.	33.70 seconds.
25 => 49.14 seconds.	34.36 seconds.
|#

;;; Pixel sampling.
;;; - A uniformly sampled box filter yields significant aliasing artifacts.
;;; - Stratified sampling of the box filter is better.
;;; - Importance sampling of a non-uniform filter appears to be best.
;;; - Stratified importance sampling...?  Difficult to implement?
#+(or)
(flet ((f (x y)
	 (+ .5 (* .5 (sin (+ (* .01 x x) (* .004 y y))))))
       (box (f n x y)
	 (/ (loop repeat n
		  sum (funcall f (+ x (random 1.0)) (+ y (random 1.0))))
	    n))
       (stratified (f n x y)
	 (/ (loop for i from 0 below n sum
		  (loop for j from 0 below n sum
			(funcall f (+ x (/ i n) (random (/ 1.0 n)))
				 (+ y (/ j n) (random (/ 1.0 n))))))
	    (* n n)))
       (importance (f n m x y)
	 (flet ((foo ()
		  (loop repeat m sum (- (random 1.0) .5))))
	   (/ (loop repeat n sum (funcall f (+ x (foo)) (+ y (foo)))) n))))
  (with-window (:width 314 :height 314)
    (loop for y from 0 below 314 do
	  (loop for x from 0 below 314
	        with n = 3
	        ;for z = (box #'f (* n n) x y)
	        ;for z = (stratified #'f n x y)
	        for z = (importance #'f (* n n) 3 x y)
		for c = (round (* 255 z))
		do (draw-point x y c c c)))))

(defun statistics (fn n)
  (let* ((samples (loop repeat n collect (funcall fn)))
	 (average (/ (reduce #'+ samples) n))
	 (variance (/ (reduce #'+ (mapcar (lambda (x)
					    (* (- x average) (- x average)))
					  samples))
		      n)))
    (values average (sqrt variance))))

(defun draw-rank-1-lattice (n)
  (with-window (:width 400 :height 400)
    (labels ((draw-cross (x y &optional (size 2))
	       (draw-line (- x size) y (+ x size 1) y)
	       (draw-line x (- y size) x (+ y size 1)))
	     (sqr (x)
	       (* x x))
	     (dist (x y)
	       (sqrt (+ (sqr x) (sqr y)))))
	(loop for k from 1 to (ceiling n 2) do
	  (let ((dist (list 2 2 2 2)))
	    (clear *canvas*)
	    (format t "~&k = ~D~%" k)
	    (dotimes (i n)
	      (let* ((y (/ i n))
		     (x (mod (* k y) 1)))
		(setf dist (sort (cons (dist x y) dist) #'<))
		(setf (cdddr dist) nil)
		(draw-cross (round (* 400 x)) (round (* 400 y)))))
	    (format t "dist = ~A~%" (rest dist))
	    (format t "closeness to triangles = ~A~%"
		    (/ (third dist) (second dist)))
	    (format t "closeness to squares = ~A~%"
		    (/ (third dist) (second dist) (sqrt 2)))
	    (format t "dist to first = ~A~%"
		    (dist (/ 1 n) (* k (/ 1 n))))
	    (format t "dist to last on first line = ~A~%"
		    (let ((i (floor (/ 1.0 (/ k n)))))
		      (dist (- 1.0 (/ i n)) (* k (/ i n)))))
	    #|
	    (format t "dist to fist on second line = ~A~%"
		    (let ((i (ceiling (/ 1.0 (/ k n)))))
		      (dist (/ i n) (* k (/ i n)))))
	    |#
	    (read-line))))))

#|
;;; Degrade luminosity with log function.
(loop for x from 0.8 to 1.5 by 0.1
     with a = 0.8
     with b = 100
     for y = (+ x (- a) (/ 1 (log b)))
     for z = (+ (log y b) a (- (log (/ 1 (log b)) b)))
     collect (list x z))

;;; Degrade luminosity with exponential function.
(loop for x from 0.8 to 1.5 by 0.1
     with a = 0.8
     with b = 0.3
     for y = (- x a -1)
     for z = (+ (- (expt y b) 1) a)
     collect (list x z))
|#

(defun xyz-to-rgb-matrix (rx ry gx gy bx by wx wy)
  (let* ((rz (- 1 rx ry))
	 (gz (- 1 gx gy))
	 (bz (- 1 bx by))
	 (wz (- 1 wx wy))
	 (xr (- (* gy bz) (* by gz)))
	 (yr (- (* bx gz) (* gx bz)))
	 (zr (- (* gx by) (* bx gy)))
	 (xg (- (* by rz) (* ry bz)))
	 (yg (- (* rx bz) (* bx rz)))
	 (zg (- (* bx ry) (* rx by)))
	 (xb (- (* ry gz) (* gy rz)))
	 (yb (- (* gx rz) (* rx gz)))
	 (zb (- (* rx gy) (* gx ry)))
	 (wr (/ (+ (* xr wx) (* yr wy) (* zr wz)) wy))
	 (wg (/ (+ (* xg wx) (* yg wy) (* zg wz)) wy))
	 (wb (/ (+ (* xb wx) (* yb wy) (* zb wz)) wy)))
    (list `(,(/ xr wr) ,(/ yr wr) ,(/ zr wr))
	  `(,(/ xg wg) ,(/ yg wg) ,(/ zg wg))
	  `(,(/ xb wb) ,(/ yb wb) ,(/ zb wb)))))

(defun matrix-determinant (matrix)
  (loop for i below 3 sum
    (loop for k in '(1 -1) for x = k sum
      (loop for i2 = i then (+ i2 k) and j below 3 do
	(setq x (* x (nth (mod i2 3) (nth j matrix))))
	finally (return x)))))

(defun invert-matrix (matrix)
  (let ((det (matrix-determinant matrix)))
    (loop for i below 3 collect
      (loop for j below 3 collect
        (loop for k in '(1 -1) for x = (/ k det) sum
          (loop for i2 = (+ i k) then (+ i2 k) and j from (1+ j) repeat 2 do
	    (setq x (* x (nth (mod i2 3) (nth (mod j 3) matrix))))
	    finally (return x)))))))

(defmacro with-transformed-color ((&key ((:from  (x y z)))
				       	((:to    (r g b)))
				       	((:red   (rx ry)))
				       	((:green (gx gy)))
				       	((:blue  (bx by)))
				       	((:white (wx wy)))
					invert)
				  &body body)
  (let ((matrix (xyz-to-rgb-matrix rx ry gx gy bx by wx wy)))
    (when invert
      (setq matrix (invert-matrix matrix)))
    `(let ,(loop for row in matrix
	         and var in (list r g b) collect
		 `(,var (+ ,@(loop for val in row
			           and var in (list x y z)
			           collect `(* ,val, var)))))
	  (declare (type fast-float ,r ,g ,b))
	  ,@body)))

(defun xyz-to-rgb (x y z)
  (with-transformed-color (:from  (x y z)
                           :to    (r g b)
                           :red   (.6400 .3300)
			   :green (.3000 .6000)
			   :blue  (.1500 .0600)
			   :white (.3127 .3290))
    (values r g b)))

#|
(defun xyz-to-rgb (x y z)
  (values (+ (*  3.240479 x) (* -1.537150 y) (* -0.498535 z))
	  (+ (* -0.969256 x) (*  1.875991 y) (*  0.041556 z))
	  (+ (*  0.056648 x) (* -0.204043 y) (*  1.057311 z))))
|#

(defun rgb-to-xyz (r g b)
  (with-transformed-color (:from  (r g b)
			   :to    (x y z)
			   :red   (.6400 .3300)
			   :green (.3000 .6000)
			   :blue  (.1500 .0600)
			   :white (.3127 .3290)
			   :invert t)
    (values x y z)))

#|
(defun rgb-to-xyz (r g b)
  (values (+ (* 0.412453 r) (* 0.357580 g) (* 0.180423 b))
	  (+ (* 0.212671 r) (* 0.715160 g) (* 0.072169 b))
	  (+ (* 0.019334 r) (* 0.119193 g) (* 0.950227 b))))
|#

(defun draw-cie-xyz (&key (white-x 0.3127) (white-y 0.3290))
  (with-window (:width 350 :height 400)
    (flet ((draw-cross (x y &optional (size 5))
	     (draw-line (- x size) y (+ x size 1) y)
	     (draw-line x (- y size) x (+ y size 1))))
      (when (and white-x white-y)
	(draw-cross (round (* 450 white-x))
		    (round (- 400 (* 450 white-y))))))
    (loop for nm from 400 to 700 by 3 do
	 (let* ((x (wavelength-response :x nm))
		(y (wavelength-response :y nm))
		(z (wavelength-response :z nm))
		(sum (+ x y z))
		(x* (/ x sum))
		(y* (/ y sum)))
	   (multiple-value-bind (r g b) (wavelength-rgb nm)
	     (multiple-value-bind (x2 y2 z2) (rgb-to-xyz r g b)
	       (let* ((sum (+ x2 y2 z2))
		      (x2* (/ x2 sum))
		      (y2* (/ y2 sum)))
		 (draw-arrow (round (* 450 x*))
			     (round (- 400 (* 450 y*)))
			     (round (* 450 x2*))
			     (round (- 400 (* 450 y2*))))))))
	 #+(or)
	 (let* ((x (wavelength-response :x nm))
		(y (wavelength-response :y nm))
		(z (wavelength-response :z nm))
		(sum (+ x y z))
		(x* (/ x sum))
		(y* (/ y sum)))
	   (let ((x1 (/ x y))
		 (z1 (/ z y))
		 (a 0.9)
		 (wx (/ white-x white-y))
		 (wz (/ (- 1 white-x white-y) white-y)))
	     (setf x (* y (+ (* a x1) (* (- 1 a) wx))))
	     (setf z (* y (+ (* a z1) (* (- 1 a) wz)))))
	   (multiple-value-bind (r g b) (xyz-to-rgb x y z)
	     ;;(setf (values r g b) (clamp-to-zero r g b))
	     ;;(setf (values r g b) (clamp-towards-grey r g b))
	     ;;(setf (values r g b) (clamp-smoothly1-towards-grey r g b nm))
	     ;;(setf (values r g b) (clamp-smoothly2-towards-grey r g b nm))
	     ;;(setf (values r g b) (clamp-uniformly-towards-grey r g b))
	     #+(or)
	     (let ((x 0.5)
		   (w (srgb-luminosity r g b)))
	       (setf r (+ (* x r) (* (- 1 x) w)))
	       (setf g (+ (* x g) (* (- 1 x) w)))
	       (setf b (+ (* x b) (* (- 1 x) w))))
	     (multiple-value-bind (x2 y2 z2) (rgb-to-xyz r g b)
	       (let* ((sum (+ x2 y2 z2))
		      (x2* (/ x2 sum))
		      (y2* (/ y2 sum)))
		 (draw-arrow (round (* 450 x*))
			     (round (- 400 (* 450 y*)))
			     (round (* 450 x2*))
			     (round (- 400 (* 450 y2*)))))))))))

(defun draw-munsell (&key (white-x 0.3127) (white-y 0.3290)
		     (width 350) (height (* 8/7 width)))
  (with-window (:width width :height height)
    (labels ((window-x (x)
	       (round (* 9/7 width x)))
	     (window-y (y)
	       (round (- height (* 9/8 height y))))
	     (draw-cross (x y &optional (size 5))
	       (draw-line (- x size) y (+ x size 1) y)
	       (draw-line x (- y size) x (+ y size 1))))
      (when (and white-x white-y)
	(draw-cross (window-x white-x) (window-y white-y)))
      (loop for nm from 400 to 700 by 0.1 do
	   (let* ((x (wavelength-response :x nm))
		  (y (wavelength-response :y nm))
		  (z (wavelength-response :z nm))
		  (sum (+ x y z))
		  (x* (/ x sum))
		  (y* (/ y sum)))
	     (draw-point (window-x x*) (window-y y*))))
      (let ((x1 (wavelength-response :x 360))
	    (y1 (wavelength-response :y 360))
	    (z1 (wavelength-response :z 360))
	    (x2 (wavelength-response :x 830))
	    (y2 (wavelength-response :y 830))
	    (z2 (wavelength-response :z 830)))
	(draw-line (window-x (/ x1 (+ x1 y1 z1)))
		   (window-y (/ y1 (+ x1 y1 z1)))
		   (window-x (/ x2 (+ x2 y2 z2)))
		   (window-y (/ y2 (+ x2 y2 z2)))))
      (loop for value in '(2) do
	(loop for hue in (remove-if-not
			  (lambda (x) (eql (aref (symbol-name x) 0) '#\5))
			  (remove-duplicates
			   (loop for (hue) in *real-munsell-renotations* collect hue))) do
	  (loop for chroma from 2 to 50 by 2
	     with x1 = nil and y1 = nil do
	     (let ((color (find-if
			   (lambda (col)
			     (destructuring-bind (h v c x y yy) col
			       (declare (ignore x y yy))
			       (and (eq h hue) (eql c chroma) (eql v value))))
			   *real-munsell-renotations*)))
	       (when color
		 (destructuring-bind (h v c x2 y2 yy) color
		   (declare (ignore h v c yy))
		   (when x1
		     (draw-line (window-x x1) (window-y y1)
				(window-x x2) (window-y y2)))
		   (setq x1 x2 y1 y2))))))))))

(defun draw-osa-sampling (&key (white-x 0.3127) (white-y 0.3290)
			  (width 350) (height (* 8/7 width)))
  (with-window (:width width :height height)
    (labels ((window-x (x)
	       (round (* 9/7 width x)))
	     (window-y (y)
	       (round (- height (* 9/8 height y))))
	     (draw-cross (x y &optional (size 5))
	       (draw-line (- x size) y (+ x size 1) y)
	       (draw-line x (- y size) x (+ y size 1))))
      (when (and white-x white-y)
	(draw-cross (window-x white-x) (window-y white-y)))
      (loop for nm from 400 to 700 by 0.1 do
	   (let* ((x (wavelength-response :x nm))
		  (y (wavelength-response :y nm))
		  (z (wavelength-response :z nm))
		  (sum (+ x y z))
		  (x* (/ x sum))
		  (y* (/ y sum)))
	     (draw-point (window-x x*) (window-y y*))))
      (let ((x1 (wavelength-response :x 360))
	    (y1 (wavelength-response :y 360))
	    (z1 (wavelength-response :z 360))
	    (x2 (wavelength-response :x 830))
	    (y2 (wavelength-response :y 830))
	    (z2 (wavelength-response :z 830)))
	(draw-line (window-x (/ x1 (+ x1 y1 z1)))
		   (window-y (/ y1 (+ x1 y1 z1)))
		   (window-x (/ x2 (+ x2 y2 z2)))
		   (window-y (/ y2 (+ x2 y2 z2)))))
      (loop for value in '(-9.16 -0.48 5.37) do
	   (loop for color in
		(remove-if-not
		 (lambda (col)
		   (destructuring-bind (l j g x y z) col
		     (declare (ignore j g x y z))
		     (= value L)))
		 *osa-ucs-radial-sampling*) do
		(destructuring-bind (l j g x y z) color
		  (declare (ignore l j g))
		  (print (list x y z))
		  (draw-cross  (window-x (/ x (+ x y z)))
			       (window-y (/ y (+ x y z))))))))))

(defun xyz-to-cielab (x y z &key (white-x 0.3127) (white-y 0.3290)
		                 (white-luminance 1.0))
  (flet ((f (x)
	   (if (> x (expt 6/29 3))
	       (expt x 1/3)
	       (+ (* 841/108 x) 4/29))))
    (let* ((wx (* white-luminance (/ white-x white-y)))
	   (wy white-luminance)
	   (wz (* white-luminance (/ (- 1 white-x white-y) white-y)))
	   (x* (/ x wx))
	   (y* (/ y wy))
	   (z* (/ z wz))
	   (fy* (f y*))
	   (L* (- (* 116 fy*) 16))
	   (a* (* 500 (- (f x*) fy*)))
	   (b* (* 200 (- fy* (f z*)))))
      (values L* a* b*))))

(defun xyz-to-cieluv (x y z &key (white-x 0.3127) (white-y 0.3290)
		                 (white-luminance 1.0))
  (flet ((u (x y z)
	   (/ (* 4 x) (+ x (* 15 y) (* 3 z))))
	 (v (x y z)
	   (/ (* 9 y) (+ x (* 15 y) (* 3 z))))
	 (f (x)
	   (if (<= x (expt 6/29 3))
	       (* (expt 29/3 3) x)
	       (- (* 116 (expt x 1/3)) 16))))
    (let* ((wx (* white-luminance (/ white-x white-y)))
	   (wy white-luminance)
	   (wz (* white-luminance (/ (- 1 white-x white-y) white-y)))
	   (L* (f (/ y wy)))
	   (u* (* 13 L* (- (u x y z) (u wx wy wz))))
	   (v* (* 13 L* (- (v x y z) (v wx wy wz)))))
      (values L* u* v*))))

#|
(defun draw-cielab-munsell (&key (white-x 0.3127) (white-y 0.3290)
			    (width 600) (height 600))
  (with-window (:width width :height height)
    (labels ((window-x (x)
	       (round (+ (* width .5) (* width .0015 x))))
	     (window-y (y)
	       (round (- (* height .5) (* height .0015 y))))
	     (draw-cross (x y &optional (size 5))
	       (draw-line (- x size) y (+ x size 1) y)
	       (draw-line x (- y size) x (+ y size 1))))
      (when (and white-x white-y)
	(multiple-value-bind (l a b) (xyz-to-cielab
				      (/ white-x white-y)
				      1.0
				      (/ (- 1 white-x white-y) white-y))
	  (draw-cross (window-x a) (window-y b))))
      (loop for nm from 400 to 700 by .1 do
	   (let ((x (wavelength-response :x nm))
		 (y (wavelength-response :y nm))
		 (z (wavelength-response :z nm)))
	     (multiple-value-bind (l a b) (xyz-to-cielab (/ x y) 1 (/ z y))
	       (draw-point (window-x a) (window-y b)))))
      (let ((x1 (wavelength-response :x 360))
	    (y1 (wavelength-response :y 360))
	    (z1 (wavelength-response :z 360))
	    (x2 (wavelength-response :x 830))
	    (y2 (wavelength-response :y 830))
	    (z2 (wavelength-response :z 830)))
	(multiple-value-bind (l1 a1 b1) (xyz-to-cielab (/ x1 y1) 1 (/ z1 y1))
	  (multiple-value-bind (l2 a2 b2) (xyz-to-cielab (/ x2 y2) 1 (/ z2 y2))
	    (draw-line (window-x a1) (window-y b1)
		       (window-x a2) (window-y b2)))))
      (loop for value in '(1 5) do
	(loop for hue in (remove-if-not
			  (lambda (x) t);(eql (aref (symbol-name x) 0) #\5))
			  (remove-duplicates
			   (loop for (hue) in *real-munsell-renotations* collect hue))) do
	  (loop for chroma from 2 to 50 by 2
	     with a1 = nil and b1 = nil do
	     (let ((color (find-if
			   (lambda (col)
			     (destructuring-bind (h v c x y yy) col
			       (and (eq h hue) (eql c chroma) (eql v value))))
			   *real-munsell-renotations*)))
	       (when color
		 (destructuring-bind (h v c x y yy) color
		   (multiple-value-bind (l a b) (xyz-to-cielab
						 (/ x y)
						 1.0
						 (/ (- 1 x y) y))
		     (when a1
		       (draw-line (window-x a1) (window-y b1)
				  (window-x a) (window-y b)))
		     (setq a1 a b1 b)))))))))))
|#

(defun draw-cie-xz (&key (white-x 0.3127) (white-y 0.3290))
  (declare (ignorable white-x white-y))
  (with-window (:width 450 :height 450)
    (loop for nm from 400 to 700 by 3 do
      (let* ((x (wavelength-response :x nm))
	     (y (wavelength-response :y nm))
	     (z (wavelength-response :z nm))
	     (x* (/ x y))
	     (z* (/ z y)))
	(multiple-value-bind (r g b) (xyz-to-rgb x* 1 z*)
	  ;;(setf (values r g b) (clamp-to-zero r g b))
	  ;;(setf (values r g b) (clamp-towards-grey r g b))
	  ;;(setf (values r g b) (clamp-smoothly1-towards-grey r g b nm))
	  ;;(setf (values r g b) (clamp-smoothly2-towards-grey r g b nm))
	  ;;(setf (values r g b) (clamp-uniformly-towards-grey r g b))
	  (multiple-value-bind (x2 y2 z2) (rgb-to-xyz r g b)
	    (let ((x2* (/ x2 y2))
		  (z2* (/ z2 y2))
		  (zoom 3))
	      #+(or)
	      (let ((a .1)
		    (wx (/ white-x white-y))
		    (wz (/ (- 1 white-x white-y) white-y)))
		(setf x2* (+ (* a x*) (* (- 1 a) wx)))
		(setf z2* (+ (* a z*) (* (- 1 a) wz))))
	      (draw-arrow (round (* zoom 450/10 x*))
			  (round (- 450 (* zoom 450/50 z*)))
			  (round (* zoom 450/10 x2*))
			  (round (- 450 (* zoom 450/50 z2*)))))))))))
;;; BT.709	x	y
;;;	R	0.64 	0.33
;;;	G	0.30 	0.60
;;;	B	0.15 	0.06
;;;	W	0.3127 	0.3290

(defun gamma-709 (x)
  (let ((1/gamma 0.45)
	(a 0.099)
	(cc 0.018))
    (if (<= x cc)
	(* 4.5 x)
	(- (* (1+ a) (expt x 1/gamma)) a))))

(defun gamma-srgb (x)
  (let ((1/gamma (/ 1 2.4))
	(a 0.055)
	(cc 0.0031308))
    (if (<= x cc)
	(* 12.92 x)
	(- (* (1+ a) (expt x 1/gamma) ) a))))

(defmacro with-monitor-colors ((r g b &key (gamma :srgb)) &body body)
  (flet ((gamma-correct (x)
	   (etypecase gamma
	     ((eql :709)	`(gamma-709 ,x))
	     ((eql :srgb)	`(gamma-srgb ,x))
	     (real		`(expt ,x ,(/ gamma))))))
    `(let ((,r (round (* 255 ,(gamma-correct r))))
	   (,g (round (* 255 ,(gamma-correct g))))
	   (,b (round (* 255 ,(gamma-correct b)))))
       (declare (type (integer 0 255) r g b))
       ,@body)))

(defun clamp-to-zero (r g b)
  (values (max r 0) (max g 0) (max b 0)))

(defun clamp-to-one (r g b)
  (values (min r 1) (min g 1) (min b 1)))

(defun clamp-towards-grey (r g b)
  (let ((w (min 0 r g b)))
    (values (- r w) (- g w) (- b w))))

(defun clamp-uniformly-towards-grey (r g b)
  (let ((w 0.3702009))
    (values (/ (+ r w) (1+ w))
	    (/ (+ g w) (1+ w))
	    (/ (+ b w) (1+ w)))))

(defun clamp-smoothly1-towards-grey (r g b nm)
  (let* ((w (typecase nm
	      ((real 435 520)
	       (+ -0.23
		  (* 0.14
		     (cos (* (/ pi (- 520 435)) (- nm 435))))))
	      ((real 520 630)
	       (+ -0.21
		  (* 0.16
		     (cos (+ (* (/ pi (- 630 520)) (- nm 520)) pi)))))
	      (t (min 0 r g b)))
	   #+(or)
	   (+ (if (<= nm 520)
		  (* 0.3702009 (sin (* (/ (/ pi 2) (- 520 400)) (- nm 400))))
		  (* 0.3702009 (sin (+ (* (/ (/ pi 2) (- 700 520)) (- nm 520))
				       (/ pi 2)))))
	      .01))
	 (x 1))
    (values (/ (- r w -0.001) x)
	    (/ (- g w -0.001) x)
	    (/ (- b w -0.001) x))))

;; 464.3 549.1 611.3
(defun clamp-smoothly2-towards-grey (r g b nm)
  (typecase nm
    ((real 444.3 484.3)
     (let* ((x (* pi (/ (- nm 464.3) 20)))
	    (y (* .015 (1+ (cos x)))))
       (incf r y)
       (incf g y)
       (incf b y)))
    ((real 529.1 569.1)
     (let* ((x (* pi (/ (- nm 549.1) 20)))
	    (y (* .04 (1+ (cos x)))))
       (incf r y)
       (incf g y)
       (incf b y)))
    ((real 591.3 5631.3)
     (let* ((x (* pi (/ (- nm 611.3) 20)))
	    (y (* .01 (1+ (cos x)))))
       (incf r y)
       (incf g y)
       (incf b y))))
  (let ((w (min 0 r g b)))
    (values (- r w)
	    (- g w)
	    (- b w))))

;;; 360 nm => 0.17556,   0.0052938
;;; 404 nm => 0.17310,   0.0047740
;;; 504 nm => 0.0036364, 0.63301
;;; 521 nm => 0.082053,  0.83409
;;; 830 nm => 0.73469,   0.26531
;;; min rgb = (-1.53542 -0.21527 -0.20194) @ ~520 nm
;;; max rgb = ( 1.97441  1.87293  1.05731) @ ~605 nm

(defun draw-cie-xy ()
  (declare (optimize debug))
  (let* ((width 600)
	 (height 600))
    (labels ((wavelength-x (nm)
	       (let ((x (wavelength-response :x nm))
		     (y (wavelength-response :y nm))
		     (z (wavelength-response :z nm)))
		 (/ x (+ x y z))))
	     (wavelength-y (nm)
	       (let ((x (wavelength-response :x nm))
		     (y (wavelength-response :y nm))
		     (z (wavelength-response :z nm)))
		 (/ y (+ x y z))))
	     (find-x (y fn nm1 nm2)
	       (loop for nm = (/ (+ nm1 nm2) 2)
		     until (< (- nm2 nm1) .00001) do
		     (if (funcall fn (wavelength-y nm) y)
			 (setf nm1 nm)
			 (setf nm2 nm))
		     finally (return (wavelength-x nm))))
	     (y-xmin (y)
	       (find-x y #'< 360 521))
	     (y-xmax (y)
	       (if (>= y (wavelength-y 830))
		   (find-x y #'> 521 830)
		   (let ((x1 (wavelength-x 360))
			 (y1 (wavelength-y 360))
			 (x2 (wavelength-x 830))
			 (y2 (wavelength-y 830)))
		     (+ (* (- x2 x1) (/ (- y y1) (- y2 y1))) x1)))))
      (with-window (:width (round (* .75 width))
		    :height (round (* .85 height)))
	(loop for y from 0 below height
	      for y* = (- .85 (/ y height))
	      for x1 = (round (* width (y-xmin y*)))
	      for x2 = (round (* width (y-xmax y*))) do
	      (draw-line 0 y x1 y)
	      (draw-line (1+ x2) y (1- width) y)
	      (loop for x from x1 to x2
		    for x* = (/ x width)
		    for z* = (- 1 x* y*) do
		    (multiple-value-bind (r g b)
			(xyz-to-rgb (* .4 (/ x* y*))
				    .4
				    (* .4 (/ z* y*)))
		      ;;#+(or)
		      (with-preserved-luminosity (r g b)
			(setf (values r g b) (clamp-towards-grey r g b)))
		      #+(or)
		      (let ((x (min r g b)))
			(decf r x)
			(decf g x)
			(decf b x))
		      #+(or)
		      (let ((x (max r g b)))
			(setf r (/ r x))
			(setf g (/ g x))
			(setf b (/ b x)))
		      (setf (values r g b) (clamp-to-one r g b))
		      (with-monitor-colors (r g b)
			(draw-point x y r g b))
		      (values r g b))))))))

(defun wavelength-rgb (nm &optional (luma (/ 2.5164869819578133)))
  (let ((x (* luma (wavelength-response :x nm)))
	(y (* luma (wavelength-response :y nm)))
	(z (* luma (wavelength-response :z nm))))
    #+(or)
    (when (< 474 nm 548)
      (setf x 0 y 0 z 0)) ;-0.08864192288965543
    #+(or)
    (let* ((white-x .3127)
	   (white-y .3290)
	   (wx (/ white-x white-y))
	   (wz (/ (- 1 white-x white-y) white-y))
	   (a 0.04333))
      (setf x (+ (* a x) (* (- 1 a) (* y wx))))
      (setf z (+ (* a z) (* (- 1 a) (* y wz)))))
    (multiple-value-bind (r g b) (xyz-to-rgb x y z)
      #+(or)
      (let ((x #+(or) 0.089 0.36757))
	(setf r (/ (+ r x) (1+ x)))
	(setf g (/ (+ g x) (1+ x)))
	(setf b (/ (+ b x) (1+ x))))
      #+(or)
      (let ((x1 0.089)
	    (x2 0.36757))
	(setf r (/ (+ r x2) (1+ x2)))
	(setf g (/ (+ g x1) (1+ x1)))
	(setf b (/ (+ b x1) (1+ x1))))
      #+(or)
      (with-preserved-luminosity (r g b)
	(setf (values r g b) (clamp-to-zero r g b)))
      ;;#+(or)
      (with-preserved-luminosity (r g b)
	(setf (values r g b) (clamp-towards-grey r g b)))
      #+(or)
      (with-preserved-luminosity (r g b)
	(setf (values r g b) (clamp-smoothly1-towards-grey r g b nm)))
      #+(or)
      (with-preserved-luminosity (r g b)
	(setf (values r g b) (clamp-uniformly-towards-grey r g b)))
      ;;#+(or)
      (setf (values r g b) (clamp-to-one r g b))
      (values r g b))))

(defun find-spectrum-locus (value fn &key (test #'<) (start 360.0) (end 830.0))
  (flet ((wavelength-x (nm)
	   (let ((x (wavelength-response :x nm))
		 (y (wavelength-response :y nm))
		 (z (wavelength-response :z nm)))
	     (/ x (+ x y z))))
	 (wavelength-y (nm)
	   (let ((x (wavelength-response :x nm))
		 (y (wavelength-response :y nm))
		 (z (wavelength-response :z nm)))
	     (/ y (+ x y z)))))
    (loop for nm = (/ (+ start end) 2)
	  for x = (wavelength-x nm)
	  for y = (wavelength-y nm)
          until (< (- end start) .001) do
	  (if (funcall test (funcall fn nm x y) value)
	      (setf start nm)
	      (setf end nm))
          finally (return (values nm x y)))))

(defun xyz-dominant-wavelength (x y z &optional (white-x 1/3) (white-y 1/3))
  (let* ((sum (+ x y z))
	 (x (/ x sum))
	 (y (/ y sum))
	 (cx (- (* 2 white-x) x))
	 (cy (- (* 2 white-y) y)))
    (if (and (= x cx) (= y cy))
	(values nil nil)
	(labels ((foo (nm x y)
		   (declare (ignore nm))
		   (mod (+ (atan (- y white-y) (- x white-x)) (* 1/2 pi))
			(* 2 pi)))
		 (maybe-nil (nm x1 y1 x2 y2)
		   (if (< 0.999 (/ (foo 0 x1 y1) (foo 0 x2 y2)) 1.001)
		       nm
		       nil)))
	  (multiple-value-bind (nm1 x1 y1)
	      (find-spectrum-locus (foo 0 x y) #'foo :test #'>)
	    (multiple-value-bind (nm2 x2 y2)
		(find-spectrum-locus (foo 0 cx cy) #'foo :test #'>)
	      (values (maybe-nil nm1 x y x1 y1)
		      (maybe-nil nm2 cx cy x2 y2))))))))

(defun excitation-purity (x y z &optional (white-x 1/3) (white-y 1/3))
  (let* ((sum (+ x y z))
	 (x (/ x sum))
	 (y (/ y sum)))
    (labels ((foo (nm x y)
	       (declare (ignore nm))
	       (mod (+ (atan (- y white-y) (- x white-x)) (* 1/2 pi))
		    (* 2 pi)))
	     (sqr (x)
	       (* x x))
	     (bar (x y)
	       (+ (sqr (- x white-x)) (sqr (- y white-y)))))
      (multiple-value-bind (nm1 x1 y1)
	  (find-spectrum-locus (foo 0 x y) #'foo :test #'>)
	(declare (ignore nm1))
	(sqrt (/ (bar x y) (bar x1 y1)))))))

(defun colorimetric-purity (x y z &optional (white-x 1/3) (white-y 1/3))
  (let* ((white-z (- 1 white-x white-y))
	 (sum (+ x y z))
	 (x (/ x sum))
	 (y (/ y sum))
	 (z (/ z sum)))
    (labels ((foo (nm x y)
	       (declare (ignore nm))
	       (mod (+ (atan (- y white-y) (- x white-x)) (* 1/2 pi))
		    (* 2 pi)))
	     (sqr (x)
	       (* x x))
	     (bar (x z)
	       (+ (sqr (- x (/ white-x white-y)))
		  (sqr (- z (/ white-z white-y))))))
      (multiple-value-bind (nm1 x1 y1)
	  (find-spectrum-locus (foo 0 x y) #'foo :test #'>)
	(declare (ignore nm1))
	(let ((z1 (- 1 x1 y1)))
	  (sqrt (/ (bar (/ x y) (/ z y))
		   (bar (/ x1 y1) (/ z1 y1)))))))))

(defun rgb-saturation (r g b)
  (let ((min (min r g b))
	(max (max r g b)))
    (if (zerop max)
	0
	(/ (- max min) max))))

(defun plot-rgb ()
  (plot-function (list (lambda (x) (nth-value 0 (wavelength-rgb x)))
		       (lambda (x) (nth-value 1 (wavelength-rgb x)))
		       (lambda (x) (nth-value 2 (wavelength-rgb x))))
		 400 700
		 :print-stats t
		 :width 400
		 :height 300
		 :colors '((255 0 0) (0 255 0) (0 0 255) (0 0 0))))

(defun plot-hsl ()
  (plot-function (list (lambda (x)
			 (/
			  (-
			   (or
			    (apply
			     #'xyz-dominant-wavelength
			     (append
			      (multiple-value-list
			       (apply
				#'rgb-to-xyz
				(multiple-value-list
				 (wavelength-rgb x))))
			      '(0.3127 0.3290)))
			    400)
			   400)
			  300))
		       (lambda (x)
			 (apply #'srgb-luminosity
				(multiple-value-list
				 (wavelength-rgb x))))
		       (lambda (x)
			 (apply
			  #'colorimetric-purity
			  (append (multiple-value-list
				   (apply
				    #'rgb-to-xyz
				    (multiple-value-list
				     (wavelength-rgb x))))
				  '(0.3127 0.3290)))))
		 400 700
		 :width 400
		 :height 300
		 :max 1.0
		 :colors '((255 0 0) (0 255 0) (0 0 255))))

(defun draw-spectrum (&key (lines-every 1000))
  (let ((width 1000)
	(height 100)
	(blue 400)
	(red 700))
    (with-window (:width width :height height)
      (loop for x below width
	    for nm from blue to red by (/ (- red blue) width) do
	    (if (< (rem nm lines-every) (/ (- red blue) width))
		(draw-line x 0 x (1- height) 0 0 0)
		(multiple-value-bind (r g b) (wavelength-rgb nm)
		  (with-monitor-colors (r g b)
		    (draw-line x 0 x (1- height) r g b))))))))

(defun lerp (x1 x2 y)
  (+ x1 (* (mod y 1) (- x2 x1))))

(defun draw-optimal-colors ()
  (let ((width 600/2)
	(height 400/2)
	(blue 400)
	(red 680))
    (with-window (:width width :height height)
      (loop for y below height
	    for w = (lerp 0.5 0 (/ y height)) do
	    (flet ((foo (fn)
		     (loop for x below width
			   for x2 = (lerp blue red (- (/ x width) w))
			   for y2 = (lerp blue red (+ (/ x width) w)) do
			   (loop for nm from blue to red
				 when (or (and (< x2 y2) (< x2 nm y2))
					  (and (> x2 y2) (not (< y2 nm x2))))
				 sum (wavelength-response :x nm) into x* and
				 sum (wavelength-response :y nm) into y* and
				 sum (wavelength-response :z nm) into z*
				 finally
				 (multiple-value-call
				  fn x y (xyz-to-rgb x* y* z*))))))
	      (let ((max 0))
		(foo (lambda (x y r g b)
		       (declare (ignore x y))
		       (setf max (max max r g b))))
		(print max)
		(when (zerop max)
		  (setq max 1))
		(foo (lambda (x y r g b)
		       (setf r (/ r max) g (/ g max) b (/ b max))
		       (with-preserved-luminosity (r g b)
			 (setf (values r g b) (clamp-towards-grey r g b)))
		       (with-monitor-colors (r g b)
			 (draw-point x y r g b))))))
	    (finish-output (ltk:wish-stream *wish*))))))

(defun draw-optimal-colors-xy ()
  (let ((width 500)
	(height 500)
	(blue 360)
	(red 830))
    (with-window (:width width :height height)
      (loop for nm1 from blue to red by 0.1 do
	    (loop for x2 in '(1 75 280 420) do
			  ;from 2 to 170 #|(- red blue)|# by 80 do
		  (do ((x* 0)
		       (y* 0)
		       (z* 0)
		       (nm nm1 (if (>= nm red) blue (1+ nm)))
		       (i 0 (1+ i)))
		      ((>= i x2)
		       (let ((sum (+ x* y* z*)))
			 (unless (zerop sum)
			   (let ((xx (* 1.15 (/ x* sum)))
				 (yy (* 1.15 (- .85 (/ y* sum)))))
			     (draw-point (round (* width xx))
					 (round (* height yy)))))))
		    (incf x* (wavelength-response :x nm))
		    (incf y* (wavelength-response :y nm))
		    (incf z* (wavelength-response :z nm))))))))

(defun draw-optimal-colors-cie-1976-ucs ()
  (let ((width 500)
	(height 500)
	(blue 360)
	(red 830)
	(xyz-to-ucs #'xyz-to-cieluv))
    (flet ((window-x (x)
	     (round (+ (* width .5) (* width .0005 x))))
	   (window-y (y)
	     (round (- (* height .5) (* height .0005 y)))))
      (with-window (:width width :height height)
	(loop for nm1 from blue to red by 0.1 do
	      (loop for x2 in '(1 100) do
		    (do ((x* 0) (y* 0) (z* 0)
			 (nm nm1 (if (>= nm red) blue (1+ nm)))
			 (i 0 (1+ i)))
			((>= i x2)
			 (multiple-value-bind (L* a* b*)
			     (funcall xyz-to-ucs x* y* z*)
			   (declare (ignore L*))
			   (draw-point (window-x a*) (window-y b*))))
		      (incf x* (wavelength-response :x nm))
		      (incf y* (wavelength-response :y nm))
		      (incf z* (wavelength-response :z nm)))))))))

(defun profile (x)
  (let ((file #.*load-pathname*))
    (eval `(,(if x 'sb-profile:profile 'sb-profile:unprofile)
	    ,@(with-open-file (s file)
	        (loop for form = (read s nil nil)
		      while form
		      when (and (consp form)
				(member (car form) '(defun defgeneric)))
		      collect (second form)))))))
