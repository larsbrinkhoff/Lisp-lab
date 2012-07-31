(defpackage #:se.brinkhoff.trivial-hdri-io
  (:use :cl)
  (:nicknames #:hdri)
  (:shadow #:read #:write)
  (:export #:read #:write))

(in-package #:hdri)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic utilities.

(defun read-int16 (stream)
  (let ((x 0))
    (setf (ldb (byte 8 0) x) (read-byte stream))
    (setf (ldb (byte 8 8) x) (read-byte stream))
  x))

(defun write-int16 (x stream)
  (write-byte (ldb (byte 8 0) x) stream)
  (write-byte (ldb (byte 8 8) x) stream)
  nil)

(defun read-int32 (stream)
  (let ((x 0))
    (setf (ldb (byte 8 0) x) (read-byte stream))
    (setf (ldb (byte 8 8) x) (read-byte stream))
    (setf (ldb (byte 8 16) x) (read-byte stream))
    (setf (ldb (byte 8 24) x) (read-byte stream))
  x))

(defun write-int32 (x stream)
  (write-byte (ldb (byte 8 0) x) stream)
  (write-byte (ldb (byte 8 8) x) stream)
  (write-byte (ldb (byte 8 16) x) stream)
  (write-byte (ldb (byte 8 24) x) stream)
  nil)

(defun read-int64 (stream)
  (let ((x 0))
    (setf (ldb (byte 8 0) x) (read-byte stream))
    (setf (ldb (byte 8 8) x) (read-byte stream))
    (setf (ldb (byte 8 16) x) (read-byte stream))
    (setf (ldb (byte 8 24) x) (read-byte stream))
    (setf (ldb (byte 8 32) x) (read-byte stream))
    (setf (ldb (byte 8 40) x) (read-byte stream))
    (setf (ldb (byte 8 48) x) (read-byte stream))
    (setf (ldb (byte 8 56) x) (read-byte stream))
    x))

(defun write-int64 (x stream)
  (write-byte (ldb (byte 8 0) x) stream)
  (write-byte (ldb (byte 8 8) x) stream)
  (write-byte (ldb (byte 8 16) x) stream)
  (write-byte (ldb (byte 8 24) x) stream)
  (write-byte (ldb (byte 8 32) x) stream)
  (write-byte (ldb (byte 8 40) x) stream)
  (write-byte (ldb (byte 8 48) x) stream)
  (write-byte (ldb (byte 8 56) x) stream)
  nil)

(defun read-float16 (stream)
  (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (let ((x (read-int16 stream)))
    (if (zerop x)
       0.0f0
       (let ((sign (ldb (byte 1 15) x))
	     (exponent (ldb (byte 5 10) x))
	     (significand (ldb (byte 10 0) x)))
	 (unless (zerop exponent)
	   (incf significand #x400))
	 (* (if (zerop sign) 1 -1)
	    (expt 2.0f0 (- exponent 25))
	    significand)))))

(defun write-float16 (x stream)
  (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (write-int16
   (if (zerop x)
       0
       (multiple-value-bind (significand exponent sign)
	   (integer-decode-float (float x 0f0))
	 (incf exponent (+ 15 23))
	 (when (minusp exponent)
	   (setf significand (ash significand exponent))
	   (setf exponent 0))
	 (let ((y (dpb (if (minusp sign) 1 0)
		       (byte 1 15)
		       (dpb exponent
			    (byte 5 10)
			    (ldb (byte 10 13) significand)))))
	   (if (> y #x7BFF)
	       #x7BFF
	       y))))
   stream))

(defun read-float32 (stream)
  (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (let ((x (read-int32 stream)))
    (if (zerop x)
       0.0f0
       (let ((sign (ldb (byte 1 31) x))
	     (exponent (ldb (byte 8 23) x))
	     (significand (ldb (byte 23 0) x)))
	 (unless (zerop exponent)
	   (incf significand #x800000))
	 (* (if (zerop sign) 1 -1)
	    (expt 2.0f0 (- exponent 127 23))
	    significand)))))

(defun write-float32 (x stream)
  (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (write-int32
   (if (zerop x)
       0
       (multiple-value-bind (significand exponent sign)
	   (integer-decode-float (float x 0f0))
	 (dpb (if (minusp sign) 1 0)
	      (byte 1 31)
	      (dpb (+ exponent 127 23)
		   (byte 8 23)
		   (ldb (byte 23 0) significand)))))
   stream))

(defun write-str (string stream)
  (loop for char across string do
	(write-byte (char-code char) stream)))

(defun read-str (stream size)
  (make-array size :element-type 'character
	      :initial-contents (loop repeat size
				   collect (code-char (read-byte stream)))))

(defun read-str0 (stream)
  (let ((list (loop for char = (read-byte stream)
		    until (zerop char)
		    collect (code-char char))))
    (make-array (length list) :element-type 'character :initial-contents list)))

(defun write-str0 (string stream)
  (write-str string stream)
  (write-byte 0 stream))

(defun write-ln (string stream)
  (write-str string stream)
  (write-byte 10 stream))

;;; 0.64 0.33 .03
;;; 0.30 0.60 .10
;;; 0.15 0.06 .79

(defun rgb-to-x (r g b)
  (+ (* 0.412453 r) (* 0.357580 g) (* 0.180423 b)))

(defun rgb-to-y (r g b)
  (+ (* 0.212671 r) (* 0.715160 g) (* 0.072169 b)))

(defun rgb-to-z (r g b)
  (+ (* 0.019334 r) (* 0.119193 g) (* 0.950227 b)))

(defun rgb-to-xyz (r g b)
  (values (rgb-to-x r g b) (rgb-to-y r g b) (rgb-to-z r g b)))

(defun xyz-to-r (x y z)
  (+ (* 3.240479 x) (* -1.537150 y) (* -0.498535 z)))

(defun xyz-to-g (x y z)
  (+ (* -0.969256 x) (* 1.875991 y) (* 0.041556 z)))

(defun xyz-to-b (x y z)
  (+ (* 0.056648 x) (* -0.204043 y) (* 1.057311 z)))

(defun xyz-to-rgb (x y z)
  (values (xyz-to-r x y z) (xyz-to-g x y z) (xyz-to-b x y z)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OpenEXR

(defclass exr-channel ()
  ((name :initarg :name :accessor exr-channel-name)
   (type :initarg :type)
   (linear :initarg :linear :initform 0)
   (x-sampling :initarg :x-sampling)
   (y-sampling :initarg :y-sampling)))

(defmethod print-object ((channel exr-channel) stream)
  (with-slots (name type linear x-sampling y-sampling) channel
    (format stream "#<exr-channel ~A ~A ~A ~A ~A>"
	    name type linear x-sampling y-sampling)))

(defun read-exr-channel (stream)
  (let ((name		(read-str0 stream)))
    (if (string= name "")
	nil
	(make-instance 'exr-channel
		       :name name
		       :type (ecase (read-int32 stream)
			       (0	:uint)
			       (1	:half)
			       (2	:float))
		       :linear (prog1 (read-byte stream)
				 (loop repeat 3 do (read-byte stream)))
		       :x-sampling (read-int32 stream)
		       :y-sampling (read-int32 stream)))))

(defun read-chlist (stream)
  (loop for channel = (read-exr-channel stream)
        while channel collect channel))

(defun write-exr-channel (channel stream)
  (with-slots (name type linear x-sampling y-sampling) channel
    (write-str0 name stream)
    (write-int32 (ecase type
		   (:uint	0)
		   (:half	1)
		   (:float	2))
		 stream)
    (write-byte linear stream)
    (write-byte 0 stream)
    (write-byte 0 stream)
    (write-byte 0 stream)
    (write-int32 x-sampling stream)
    (write-int32 y-sampling stream)))
    
(defun exr-channel-size (channel)
  (+ (length (exr-channel-name channel)) 17))
  
(deftype list-of (&rest types)
  (if (endp types)
      'null
      `(cons ,(first types) (list-of ,@(rest types)))))

(defun exr-type (value)
  (etypecase value
    ;;?			:|half|)
    (single-float	:|float|)
    (double-float	:|double|)
    (string		:|string|)
    ((list-of single-float single-float)
			:|v2f|)
    ((list-of single-float single-float
	      single-float single-float
	      single-float single-float
	      single-float single-float)
			:|chromaticities|)
    ((list-of integer integer integer integer)
			:|box2i|)
    ((cons exr-channel list)
			:|chlist|)
    ((member :increasing-y :decreasing-y :random-y)
			:|lineOrder|)
    ((member :no)
			:|compression|)))

(defun exr-size (value)
  (ecase (exr-type value)
    (:|half|		2)
    (:|float|		4)
    (:|double|		8)
    (:|box2i|		16)
    (:|v2f|		8)
    (:|chromaticities|	32)
    (:|chlist|		(1+ (loop for x in value
			       sum (exr-channel-size x))))
    (:|string|		(length value))
    ((:|compression| :|lineOrder|)
			1)))

(defun read-exr-attr (stream)
  (let ((name (read-str0 stream)))
    (if (string= name "")
	nil
	(let ((type (read-str0 stream))
	      (size (read-int32 stream)))
	  (cons name
		(cond
		  ((string= type "chlist")	(read-chlist stream))
		  ((string= type "string")	(read-str stream size))
		  ((string= type "chromaticities")
						(loop repeat 8
						      collect
						      (read-float32 stream)))
		  ((string= type "compression") (ecase (read-byte stream)
						  (0 :no)
						  (1 :rle)
						  (2 :zips)
						  (3 :zip)
						  (4 :piz)
						  (5 :pxr24)
						  (6 :b44)
						  (7 :b44a)))
		  ((string= type "lineOrder")	(ecase (read-byte stream)
						  (0 :increasing-y)
						  (1 :decreasing-y)
						  (2 :random-y)))
		  ((string= type "box2i")	(list (read-int32 stream)
						      (read-int32 stream)
						      (read-int32 stream)
						      (read-int32 stream)))
		  ((string= type "float")	(read-float32 stream))
		  ((string= type "v2f")		(list (read-float32 stream)
						      (read-float32 stream)))
		  (t (loop repeat size do (read-byte stream)) type)))))))

(defun write-exr-attr (stream name value)
  (let ((type (exr-type value)))
    (write-str0 name stream)
    (write-str0 (symbol-name type) stream)
    (write-int32 (exr-size value) stream)
    (ecase type
      ;;(:|half|	(write-half value stream))
      (:|float|		(write-float32 value stream))
      ;;(:|double|	(write-double value stream))
      (:|string|	(write-str value stream))
      (:|box2i|		(dolist (x value)
			  (write-int32 x stream)))
      ((:|v2f| :|v3f| :|chromaticities|)
			(dolist (x value)
			  (write-float32 x stream)))
      (:|compression|	(write-byte (ecase value
				      (:no	0))
				    stream))
      (:|lineOrder|	(write-byte (ecase value
				      (:increasing-y	0)
				      (:decreasing-y	1)
				      (:random-y	2))
				    stream))
      (:|chlist|	(dolist (x value)
			  (write-exr-channel x stream))
			(write-byte 0 stream)))
    nil))

(defun write-exr-scanline (stream y array width channels pixel-size)
  (write-int32 y stream)
  (write-int32 (* width channels pixel-size) stream)
  (loop for offset from (1- channels) downto 0 do
    (dotimes (x width)
      (ecase pixel-size
	(2  (write-float16 (aref array (+ (* channels x) offset) y) stream))
	(4  (write-float32 (aref array (+ (* channels x) offset) y) stream)))))
  nil)

(defun write-exr (stream array color &optional description)
  (let ((pixel-type :half)
	(pixel-size 2)
	(width (/ (array-dimension array 0) 3))
	(height (array-dimension array 1))
	(channels 3))
    (write-int32 20000630 stream)	;magic
    (write-int32 #x00000002 stream)	;version, scanlines
    (flet ((make-channel (name type &optional (linear 0)
			       (x-sampling 1) (y-sampling 1))
	     (make-instance 'exr-channel
			    :name name
			    :type type
			    :linear linear
			    :x-sampling x-sampling
			    :y-sampling y-sampling)))
      (if (eq color :luma)
	  (write-exr-attr stream "channels"
			  (list (make-channel "Y" pixel-type 1)))
	  (write-exr-attr stream "channels"
			  (list (make-channel "B" pixel-type 1)
				(make-channel "G" pixel-type 1)
				(make-channel "R" pixel-type 1)))))
    (ecase color
      (:luma	(setf width (* 3 width) channels 1))
      (:xyz	(write-exr-attr stream "chromaticities"
				'(1.0f0 0.0f0
				  0.0f0 1.0f0
				  0.0f0 0.0f0
				  #.(/ 1.0f0 3) #.(/ 1.0f0 3))))
      (:rgb	(write-exr-attr stream "chromaticities"
				'(0.6400f0 0.3300f0
				  0.3000f0 0.6000f0
				  0.1500f0 0.0600f0
				  0.3127f0 0.3290f0))))
    (write-exr-attr stream "compression" :no)
    (write-exr-attr stream "dataWindow" (list 0 0 (1- width) (1- height)))
    (write-exr-attr stream "displayWindow" (list 0 0 (1- width) (1- height)))
    (write-exr-attr stream "lineOrder" :increasing-y)
    (write-exr-attr stream "pixelAspectRatio" 1.0f0)
    (write-exr-attr stream "screenWindowCenter" '(0.0f0 0.0f0))
    (write-exr-attr stream "screenWindowWidth" 1.0f0)
    (when description
      (write-exr-attr stream "description" description))
    (write-byte 0 stream)
    (let ((pos (file-position stream)))
      (incf pos (* height 8))
      (dotimes (y height)
	(write-int64 pos stream)
	(incf pos (+ (* width channels pixel-size) 8))))
    (dotimes (y height)
      (write-exr-scanline stream y array width channels pixel-size))
    nil))

(defun read-exr (stream)
  (let ((x (read-int32 stream)))
    (unless (= x 20000630)
      (error "Bad OpenEXR magic value: ~D (#x~X)" x x)))
  (let ((x (read-int32 stream)))
    (case x
      (2) ;ok
      (#x202	(error "Unsupported tiled file."))
      (t	(error "Unsupported version ~D, options #x~X"
		       (logand x #xFF) (ash x -8)))))
  (let ((attributes
	 (loop for attr = (read-exr-attr stream)
	    while attr collect attr)))
    (print attributes)
    (let ((compression (cdr (assoc "compression" attributes :test #'string=))))
      (unless (eq compression :no)
	(error "Unsupported compression type: ~A" compression)))
    (let ((data-window (assoc "dataWindow" attributes :test #'string=)))
      (destructuring-bind (x0 y0 x1 y1) (cdr data-window)
	(loop with image = (make-array (list (* 3 (1+ (- x1 x0)))
					     (1+ (- y1 y0)))
				       :element-type 'single-float
				       :initial-element 0.0f0)
	      for y from y0 to y1
	      for file-position = (read-int64 stream) do
	      (let ((old-position (file-position stream)))
		(file-position stream file-position)
		(read-int32 stream) ;y
		(read-int32 stream) ;size
		(loop for x from x0 to x1 do
		     (loop for i below 3 do
			  (setf (aref image (+ (* 3 x) i) y)
				(read-float16 stream))))
		(file-position stream old-position))
	      finally (return image))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PFS, portable float stream.
;;; http://pfstools.sourceforge.net/papers/pfs_format_spec.pdf

(defun write-pfs (stream array color &optional description)
  (check-type color (member :xyz :luma))
  (write-ln "PFS1" stream)
  (let* ((width (/ (array-dimension array 0) 3))
	 (height (array-dimension array 1))
	 (channel-list '("X" "Y" "Z"))
	 (channels (length channel-list)))
    (when (eq color :luma)
      (setf width (* 3 width))
      (setf channel-list '("Y"))
      (setf channels 1))
    (write-ln (format nil "~D ~D" width height) stream)
    ;; channeCount
    (write-ln (format nil "~D" channels) stream)
    (let ((tags nil))
      (when description
	(push (format nil "DESCRIPTION=~A" description) tags))
      ;; frameTagCount
      (write-ln (format nil "~A" (length tags)) stream)
      (dolist (tag tags)
	(write-ln tag stream)))
    ;; channels
    (dolist (x channel-list)
      ;; channelName
      (write-ln x stream)
      ;; channelTagCount
      (write-ln "0" stream))
    (write-str "ENDH" stream)
    (dotimes (offset channels)
      (dotimes (y height)
	(dotimes (x width)
	  (write-float32 (aref array (+ (* channels x) offset) y) stream))))
    nil))

(defun read-pfs (stream)
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HDR, Radiance RGBE.

(defun write-hdr-pixel (x1 x2 x3 stream)
  (setf x1 (float x1 0f0))
  (setf x2 (float x2 0f0))
  (setf x3 (float x3 0f0))
  (let ((max (max x1 x2 x3)))
    (if (<= max 1e-38)
	(write-int32 0 stream)
        (multiple-value-bind (x e s) (decode-float max)
          (declare (ignore s))
          (let ((y (* x (/ 256.0 max))))
	    (write-byte 0 #+(or)(floor (* y x1)) stream)
	    (write-byte (floor (* y x2)) stream)
	    (write-byte 0 #+(or)(floor (* y x3)) stream)
	    (write-byte (+ e 128) stream)))))
  nil)

(defun write-hdr (stream array color &optional description)
  (let ((width (/ (array-dimension array 0) 3))
	(height (array-dimension array 1)))
    (write-ln "#?RADIANCE" stream)
    (ecase color
      (:xyz	(write-ln "FORMAT=32-bit_rle_xyze" stream))
      (:rgb	(write-ln "FORMAT=32-bit_rle_rgbe" stream))
      (:luma	(write-ln "FORMAT=32-bit_rle_rgbe" stream)
		(setf width (* 3 width))))
    (when description
      (write-str "DESCRIPTION=" stream)
      (write-ln description stream))
    (write-ln "" stream)
    (write-ln (format nil "-Y ~D +X ~D" height width) stream)
    (let ((write-pixel
	   (ecase color
	     ((:rgb :xyz)	(lambda (x y)
				  (write-hdr-pixel (aref array (+ (* 3 x) 0) y)
						   (aref array (+ (* 3 x) 1) y)
						   (aref array (+ (* 3 x) 2) y)
						   stream)))
	     (:luma		(lambda (x y)
				  (write-hdr-pixel (aref array x y)
						   (aref array x y)
						   (aref array x y)
						   stream))))))
      (dotimes (y height)
	(dotimes (x width)
	  (funcall write-pixel x y)))
      nil)))

(defun read-hdr (stream)
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PFM, portable float map.
;;; http://netpbm.sourceforge.net/doc/pfm.html

(defun write-pfm (stream array color &optional description)
  (declare (ignore description))
  (let ((width (array-dimension array 0))
	(height (array-dimension array 1)))
    (ecase color
      (:luma		(write-ln "Pf" stream))
      ((:rgb :xyz)	(write-ln "PF" stream)
			(setf width (/ width 3))))
    (write-ln (format nil "~A ~A" width height) stream)
    (write-ln "-1.0" stream)
    (let ((write-pixel
	   (ecase color
	     (:rgb	(lambda (x y)
			  (write-float32 (aref array (+ (* 3 x) 0) y) stream)
			  (write-float32 (aref array (+ (* 3 x) 1) y) stream)
			  (write-float32 (aref array (+ (* 3 x) 2) y) stream)))
	     (:xyz	(lambda (x y)
			  (multiple-value-bind (r g b)
			      (xyz-to-rgb
			       (aref array (+ (* 3 x) 0) y)
			       (aref array (+ (* 3 x) 1) y)
			       (aref array (+ (* 3 x) 2) y))
			    (write-float32 r stream)
			    (write-float32 g stream)
			    (write-float32 b stream))))
	     (:luma	(lambda (x y)
			  (write-float32 (aref array x y) stream))))))
      (dotimes (y height)
	(dotimes (x width)
	  (funcall write-pixel x y)))
      nil)))

(defun read-pfm (stream)
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hdri-type (x)
  (let ((type (string-downcase (pathname-type x))))
    (cond
      ((string= type "exr")	:exr)
      ((string= type "hdr")	:hdr)
      ((string= type "pfs")	:pfs)
      ((string= type "pfm")	:pfm)
      (t			nil))))

(defun call-with-stream (fn output &key direction)
  (if (streamp output)
      (funcall fn output)
      (with-open-file (stream output
			      :direction direction
			      :if-exists :supersede
			      :element-type '(unsigned-byte 8))
	(funcall fn stream))))

(defun hdri:write (output image &key (color :xyz) description type)
  (check-type output (or pathname stream string))
  (check-type image (or array list))
  (check-type color (member :rgb :xyz :luma))
  (check-type description (or string null))
  (call-with-stream
   (lambda (stream)
     (case (or type (hdri-type output))
       ((:openexr :exr)	(write-exr stream image color description))
       ((:hdr :rgbe)	(write-hdr stream image color description))
       (:pfs		(write-pfs stream image color description))
       (:pfm		(write-pfm stream image color description))
       (t		(error "Unknown HDRI type: ~A" type))))
   output :direction :output))

(defun hdri:read (input &key type)
  (check-type input (or pathname stream string))
  (call-with-stream
   (lambda (stream)
     (case (or type (hdri-type input))
       ((:openexr :exr)	(read-exr stream))
       ((:hdr :rgbe)	(read-hdr stream))
       (:pfs		(read-pfs stream))
       (:pfm		(read-pfm stream))
       (t		(error "Unknown HDRI type: ~A" type))))
   input :direction :input))

#|		Caustic4	Spectrum
pfsglview
	exr	bluish ball	violet-brown-green-blue
	hdr	ok		blue-green-yellow-brown
	pfs	reddish		blue-treen-yellow-red

qtpfsgui
	exr	bluish ball	same
	hdr	ok		same
	pfs	black ball	all black

exrdisplay
	exr	more bluish
|#
