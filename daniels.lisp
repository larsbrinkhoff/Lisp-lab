(defpackage :se.brinkhoff.daniels
  (:use :cl))

(in-package :se.brinkhoff.daniels)

(se.brinkhoff.time-syntax::enable-number-syntax)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun min->dec (minutes)
    (ecase (count #\: minutes)
      (0 (values (read-from-string minutes)))
      (1 (let ((colon (position #\: minutes)))
	   (+ (read-from-string minutes t nil :end colon)
	      (/ (read-from-string minutes t nil :start (1+ colon)) 60.0))))
      (2 (let ((colon (position #\: minutes)))
	   (+ (* 60 (read-from-string minutes t nil :end colon))
	      (min->dec (subseq minutes (1+ colon)))))))))

(defun dec->min (minutes &optional round)
  (case round
    ((nil) (setq round 2))
    ((t)   (setq round 0)))
  (setq minutes (/ (round (* 60 (expt 10 round) minutes)) 60 (expt 10 round)))
  (multiple-value-bind (x y) (floor minutes)
    (if (>= x 60)
	(multiple-value-bind (hours x) (floor x 60)
	  (format nil "~D:~2,'0D:~4,1,,,'0F" hours x (* 60 y)))
	(if (eql round 0)
	    (format nil "~D:~2,'0D" x (round (* 60 y)))
	    (format nil "~D:~V,V,,,'0F" x (+ round 3) round (* 60 y))))))

(defun minutes (x &optional round)
  (if (minusp x)
      (concatenate 'string "-" (dec->min (- x) round))
      (dec->min x round)))

(defun mins (stream x &optional colonp atsignp)
  (declare (ignore colonp atsignp))
  (write-string (dec->min x) stream))

(defvar *e-pace*
  (vector
   (min->dec "7:52")	;30
   (min->dec "7:41")
   (min->dec "7:30")
   (min->dec "7:20")
   (min->dec "7:10")
   (min->dec "7:01")
   (min->dec "6:52")
   (min->dec "6:43")
   (min->dec "6:35")
   (min->dec "6:27")
   (min->dec "6:19")	;40
   (min->dec "6:12")
   (min->dec "6:05")	;42
   (min->dec "5:58")
   (min->dec "5:52")	;44
   (min->dec "5:46")
   (min->dec "5:40")	;46
   (min->dec "5:34")
   (min->dec "5:28")	;48
   (min->dec "5:23")
   (min->dec "5:18")	;50
   (min->dec "5:13")
   (min->dec "5:08")
   (min->dec "5:04")
   (min->dec "4:59")
   (min->dec "4:55")
   (min->dec "4:50")
   (min->dec "4:46")
   (min->dec "4:42")
   (min->dec "4:38")
   (min->dec "4:35")	;60
   (min->dec "4:31")
   (min->dec "4:27")
   (min->dec "4:24")
   (min->dec "4:21")
   (min->dec "4:18")
   (min->dec "4:14")
   (min->dec "4:11")
   (min->dec "4:08")
   (min->dec "4:05")
   (min->dec "4:02")	;70
   (min->dec "4:00")
   (min->dec "3:57")
   (min->dec "3:54")
   (min->dec "3:52")
   (min->dec "3:49")
   (min->dec "3:47")
   (min->dec "3:44")
   (min->dec "3:42")
   (min->dec "3:40")
   (min->dec "3:38")	;80
   (min->dec "3:35")
   (min->dec "3:33")
   (min->dec "3:31")
   (min->dec "3:29")
   (min->dec "3:27")))	;85

(defvar *m-pace*
  (vector
   (min->dec "6:51")	;30
   (min->dec "6:41")
   (min->dec "6:31")
   (min->dec "6:21")
   (min->dec "6:13")
   (min->dec "6:04")
   (min->dec "5:56")
   (min->dec "5:48")
   (min->dec "5:41")
   (min->dec "5:33")
   (min->dec "5:27")	;40
   (min->dec "5:20")
   (min->dec "5:14")	;42
   (min->dec "5:08")
   (min->dec "5:02")	;44
   (min->dec "4:56")
   (min->dec "4:51")	;46
   (min->dec "4:46")
   (min->dec "4:41")	;48
   (min->dec "4:36")
   (min->dec "4:31")	;50
   (min->dec "4:27")
   (min->dec "4:22")
   (min->dec "4:18")
   (min->dec "4:14")
   (min->dec "4:10")
   (min->dec "4:06")
   (min->dec "4:03")
   (min->dec "3:59")
   (min->dec "3:55")
   (min->dec "3:52")	;60
   (min->dec "3:49")
   (min->dec "3:46")
   (min->dec "3:43")
   (min->dec "3:40")
   (min->dec "3:37")
   (min->dec "3:34")
   (min->dec "3:31")
   (min->dec "3:28")
   (min->dec "3:26")
   (min->dec "3:23")	;70
   (min->dec "3:21")
   (min->dec "3:19")
   (min->dec "3:16")
   (min->dec "3:14")
   (min->dec "3:12")
   (min->dec "3:10")
   (min->dec "3:07")
   (min->dec "3:05")
   (min->dec "3:03")
   (min->dec "3:01")	;80
   (min->dec "2:59")
   (min->dec "2:57")
   (min->dec "2:56")
   (min->dec "2:54")
   (min->dec "2:52")))	;85

(defvar *t-pace*
  (vector
   (min->dec "6:24")	;30
   (min->dec "6:14")
   (min->dec "6:05")
   (min->dec "5:56")
   (min->dec "5:48")
   (min->dec "5:40")
   (min->dec "5:33")
   (min->dec "5:25")
   (min->dec "5:19")
   (min->dec "5:12")
   (min->dec "5:06")	;40
   (min->dec "5:00")
   (min->dec "4:54")	;42
   (min->dec "4:49")
   (min->dec "4:43")	;44
   (min->dec "4:38")
   (min->dec "4:33")	;46
   (min->dec "4:29")
   (min->dec "4:24")	;48
   (min->dec "4:20")
   (min->dec "4:15")	;50
   (min->dec "4:11")
   (min->dec "4:07")
   (min->dec "4:04")
   (min->dec "4:00")
   (min->dec "3:56")
   (min->dec "3:53")
   (min->dec "3:50")
   (min->dec "3:45")
   (min->dec "3:43")
   (min->dec "3:40")	;60
   (min->dec "3:37")
   (min->dec "3:34")
   (min->dec "3:32")
   (min->dec "3:29")
   (min->dec "3:26")
   (min->dec "3:24")
   (min->dec "3:21")
   (min->dec "3:19")
   (min->dec "3:16")
   (min->dec "3:14")	;70
   (min->dec "3:12")
   (min->dec "3:10")
   (min->dec "3:08")
   (min->dec "3:06")
   (min->dec "3:04")
   (min->dec "3:02")
   (min->dec "3:00")
   (min->dec "2:58")
   (min->dec "2:56")
   (min->dec "2:54")	;80
   (min->dec "2:53")
   (min->dec "2:51")
   (min->dec "2:49")
   (min->dec "2:48")
   (min->dec "2:46")))	;85

(defvar *i-pace*
  (vector
   (/ (min->dec "2:22") .4)	;30
   (/ (min->dec "2:18") .4)
   (/ (min->dec "2:14") .4)
   (/ (min->dec "2:11") .4)
   (/ (min->dec "2:08") .4)
   (/ (min->dec "2:05") .4)
   (min->dec "5:07")
   (min->dec "5:00")
   (min->dec "4:54")
   (min->dec "4:48")
   (min->dec "4:42")	;40
   (min->dec "4:36")
   (min->dec "4:31")	;42
   (min->dec "4:26")
   (min->dec "4:21")	;44
   (min->dec "4:16")
   (min->dec "4:12")	;46
   (min->dec "4:07")
   (min->dec "4:03")	;48
   (min->dec "3:59")
   (min->dec "3:55")	;50
   (min->dec "3:51")
   (min->dec "3:48")
   (min->dec "3:44")
   (min->dec "3:41")
   (min->dec "3:37")
   (min->dec "3:34")
   (min->dec "3:31")
   (min->dec "3:28")
   (min->dec "3:25")
   (min->dec "3:23")	;60
   (min->dec "3:20")
   (min->dec "3:17")
   (min->dec "3:15")
   (min->dec "3:12")
   (min->dec "3:10")
   (min->dec "3:08")
   (min->dec "3:05")
   (min->dec "3:03")
   (min->dec "3:01")
   (min->dec "2:59")	;70
   (min->dec "2:57")
   (min->dec "2:55")
   (min->dec "2:53")
   (min->dec "2:52")
   (min->dec "2:49")
   (min->dec "2:48")
   (min->dec "2:46")
   (min->dec "2:44")
   (min->dec "2:42")
   (min->dec "2:41")	;80
   (min->dec "2:39")
   (min->dec "2:38")
   (min->dec "2:36")
   (min->dec "2:35")
   (min->dec "2:33")))	;85

(defvar *r-pace*
  (vector
   (/ (min->dec "2:16") .4)	;30
   (/ (min->dec "2:12") .4)
   (/ (min->dec "2:08") .4)
   (/ (min->dec "2:05") .4)
   (/ (min->dec "2:02") .4)
   (/ (min->dec "1:59") .4)
   (/ (min->dec "1:55") .4)
   (/ (min->dec "1:53") .4)
   (/ (min->dec "1:50") .4)
   (/ (min->dec "1:48") .4)
   (/ (min->dec "1:46") .4)	;40
   (/ (min->dec "1:44") .4)
   (/ (min->dec "1:42") .4)	;42
   (/ (min->dec "1:40") .4)
   (/ 98 60 .4)			;44
   (/ 96 60 .4)
   (/ 94 60 .4)			;46
   (/ 92 60 .4)
   (/ 90 60 .4)			;48
   (/ 89 60 .4)
   (/ 87 60 .4)			;50
   (/ 86 60 .4)
   (/ 85 60 .4)
   (/ 84 60 .4)
   (/ 82 60 .4)
   (/ 81 60 .4)
   (/ 80 60 .4)
   (/ 79 60 .4)
   (/ 77 60 .4)
   (/ 76 60 .4)
   (/ (min->dec "2:30") .8)	;60
   (/ (min->dec "2:28") .8)
   (/ (min->dec "2:26") .8)
   (/ (min->dec "2:24") .8)
   (/ (min->dec "2:22") .8)
   (/ (min->dec "2:20") .8)
   (/ (min->dec "2:18") .8)
   (/ (min->dec "2:16") .8)
   (/ (min->dec "2:14") .8)
   (/ (min->dec "2:12") .8)
   (/ (min->dec "2:10") .8)	;70
   (/ (min->dec "2:08") .8)
   (/ (min->dec "2:06") .8)
   (/ (min->dec "2:05") .8)
   (/ (min->dec "2:04") .8)
   (/ (min->dec "2:03") .8)
   (/ (min->dec "2:02") .8)
   (/ (min->dec "2:00") .8)
   (/ (min->dec "1:59") .8)
   (/ (min->dec "1:58") .8)
   (/ (min->dec "1:56") .8)	;80
   (/ (min->dec "1:55") .8)
   (/ (min->dec "1:54") .8)
   (/ (min->dec "1:53") .8)
   (/ (min->dec "1:52") .8)
   (/ (min->dec "1:51") .8)))	;85

(defvar *studenternas-omvandlingstabell*
  ;; 800m    1000m   1500m  3000m  5000m  10km   21,1km   LL30km   42,2km
  '((1:53.2  2:27.2  3:53    8:30  14:50  31:20  1:10:00  1:45:00  2:27:40)
    (1:56.4  2:31.4  4:00    8:45  15:15  32:10  1:11:45  1:47:30  2:31:10)
    (1:59.6  2:35.6  4:07    9:00  15:40  33:00  1:13:30  1:50:00  2:35:00)
    (2:02.8  2:39.8  4:14    9:15  16:05  33:50  1:15:15  1:53:00  2:39:00)
    (2:06.0  2:44    4:21    9:30  16:30  34:40  1:17:00  1:56:30  2:44:00)
    (2:09.2  2:48.2  4:28    9:45  17:00  35:40  1:19:00  2:01:00  2:50:00)
    (2:12.4  2:52.4  4:35   10:00  17:28  36:40  1:21:00  2:06:00  2:57:00)
    (2:15.6  2:56.6  4:42   10:15  17:56  37:45  1:23:15  2:11:00  3:04:00)
    (2:18.8  3:00.8  4:49   10:30  18:24  38:50  1:25:30  2:16:00  3:11:00)
    (2:22.0  3:05    4:56   10:45  18:52  39:55  1:28:00  2:21:00  3:18:00)
    (2:25.2  3:09.2  5:03   11:00  19:20  41:00  1:30:30  2:26:00  3:25:00)
    (2:28.4  3:13.4  5:10   11:16  19:50  42:05  1:33:00  2:31:00  3:32:00)
    (2:31.6  3:17.6  5:17   11:32  20:20  43:10  1:35:45  2:36:00  3:40:00)
    (2:34.8  3:21.8  5:24   11:48  20:50  44:15  1:38:30  2:42:00  3:48:00)
    (2:38.0  3:26.2  5:31.5 12:04  21:20  45:20  1:41:15  2:48:00  3:56:00)
    (2:41.2  3:30.6  5:39   12:20  21:50  46:30  1:44:00  2:54:00  4:04:00)
    (2:44.4  3:35.0  5:46.5 12:36  22:20  47:40  1:47:00  3:00:00  4:12:00)
    (2:47.6  3:39.4  5:54   12:52  22:50  48:50  1:50:00  3:06:00  4:20:00)
    (2:50.8  3:43.8  6:01.5 13:08  23:20  50:00  1:53:00  3:12:00  4:28:00)
    (2:54.0  3:48.2  6:09   13:24  23:50  51:10  1:56:00  3:18:00  4:36:00)
    (2:57.2  3:52.6  6:16.5 13:40  24:20  52:20  1:59:00  3:24:00  4:44:00)
    (3:00.4  3:57.0  6:23   13:56  24:50  53:30  2:02:00  3:30:00  4:52:00)
    (3:03.6  4:01.4  6:30.5 14:12  25:20  54:40  2:06:00  3:35:00  5:00:00)
    (3:06.8  4:05.8  6:38   14:28  25:50  55:50  2:10:00  3:40:00  5:10:00)))
  ;;    3.2     4.4     7.5    16     30   1:10     4:00     6:00    10:00
  ;; 3:10.0  4:10.2  6:45   14:44  26:20  57:00  2:14:00  3:46:00  5:20:00
  ;; 3:13.2  4:14.6  6:53   15:00  26:50  58:10  2:18:00  3:52:00  5:30:00
  ;; 3:16.4  4:19.0  7:00   15:16  27:20  59:20  2:22:00  3:58:00  5:40:00
  ;; 3:19.6  4:23.4  7:08   15:32  27:50  60:30  2:26:00  4:04:00  5:50:00
  ;; 3:22.8  4:27.8  7:15   15:48  28:20  61:40  2:30:00  4:10:00  6:00:00
  ;; 3:26.0  4:32.2  7:23   16:04  28:50  62:50  2:34:00  4:16:00  6:10:00

(defun studenternas-tabell (&optional (steps 1))
  (loop for t800   from    1:53.2 by (/ 0:03.2 steps)
        and t1000  from    2:27.2 by (/ (-  4:05.8  2:27.2) 23 steps)
        and t1500  from    3:53   by (/ (-  6:16.5  3:53)   20 steps)
        and t3000  from    8:30   by (/ (- 14:28    8:30)   23 steps)
        and t5000  from   14:50   by (/ (- 25:50   14:50)   23 steps)
        and t10000 from   31:20   by (/ (- 55:50   31:20)   23 steps)
        ;; 5.9 0.4
        ;; 5.73 0.5
        ;; 6.5 -0.2
        with p1 = 6.6 and p2 = -0.3
        ;;with p1 = 6.0 and p2 = 0.5
        for diff = (+ (* p1 (log (- (* 18.75 t800) 32.375) 10)) p2)
        for tM     = 2:27:40 then (+ tM (/ diff steps))
        ;;and table  in *studenternas-omvandlingstabell*
        repeat (1+ (* 23 steps)) do
        (format t "~A  ~A  ~A  ~A  ~A  ~A~%"
		(minutes t800)
		(minutes t1000)
		(minutes t1500)
		(minutes t3000)
		(minutes t5000)
		(minutes t10000))))

(defun vo2 (meters minutes)
  (when (stringp minutes)
    (setf minutes (min->dec minutes)))
  (let ((v (/ (float meters) minutes)))
    (+ -4.60 (* 0.182258 v) (* 0.000104 v v))))

(defun %vo2max (minutes)
  (when (stringp minutes)
    (setf minutes (min->dec minutes)))
  (let ((x1 (exp (* -0.012778 minutes)))
	(x2 (exp (* -0.1932605 minutes))))
    (+ 0.8 (* 0.1894393 x1) (* 0.2989558 x2))))

(defun vo2max (meters minutes)
  (/ (vo2 meters minutes) (%vo2max minutes)))

(defun vvo2max (vo2max)
  (+ 29.54 (* 5.000663 vo2max) (* -0.007546 vo2max vo2max)))

(defun vdot (meters minutes)
  (let ((v (/ (float meters) minutes))
	(x1 (exp (* -0.012778 minutes)))
	(x2 (exp (* -0.1932605 minutes))))
    (/ (+ -4.60 (* 0.182258 v) (* 0.000104 v v))
       (+ 0.8 (* 0.1894393 x1) (* 0.2989558 x2)))))

(defun pace (vdot table)
  (let* ((i (- vdot 30))
	 (i1 (floor i))
	 (i2 (1+ i1))
	 (x1 (aref table i1))
	 (x2 (aref table i2))
	 (y1 (- i2 i))
	 (y2 (- i i1)))
    (+ (* x1 y1) (* x2 y2))))

(defun e-pace (vdot)
  (pace vdot *e-pace*))

(defun m-pace (vdot)
  (pace vdot *m-pace*))

;;; 20 min => max for 67.5 minutes
;;; 25 min => .988 (.9856 - .9898 : 0.0042)  82.5
;;; 30 min => .980 (.9770 - .9831 : 0.0061)  92.5
;;; 35 min => .974 (.9712 - .9775 : 0.0063) 100
;;; 40 min => .968 (.9651 - .9700 : 0.0049) 112.5
;;; 45 min => .963 (.9595 - .9652 : 0.0057) 122.5
;;; 50 min => .956 (.9534 - .9600 : 0.0066) 135?
;;; 55 min => .952 (.9463 - .9553 : 0.0090) 150?
;;; 60 min => .947 (.9417 - .9520 : 0.0103) 160-170?

(defun t-pace (vdot)
  (pace vdot *t-pace*))

(defun i-pace (vdot)
  (pace vdot *i-pace*))

(defun r-pace (vdot)
  (pace vdot *r-pace*))

(defun newton-raphson (f x0)
  (let* ((y (funcall f x0))
	 (x1 (* x0 1.001))
	 (y1 (funcall f x1))
	 (x (- x0 (/ y (/ (- y1 y) (- x1 x0))))))
    (if (< (abs (1- (/ x0 x))) .001)
	x0
	(newton-raphson f x))))

(defun vo2max->meters (vo2max minutes)
  (when (stringp minutes)
    (setf minutes (min->dec minutes)))
  (newton-raphson (lambda (x) (- (vo2max x minutes) vo2max)) 100.0))

(defun vo2max->minutes (vo2max meters)
  (newton-raphson (lambda (x) (- (vo2max meters x) vo2max)) 1.0))

(defun %hr->%vo2max (x)
  (/ (- x 37) .64))

(defun %vo2max->%hr (x)
  (+ (* .64 x) 37))

(defun splits (tempo length &key (split 100) (start split))
  (when (stringp tempo)
    (setf tempo (min->dec tempo)))
  (loop while (<= start length) do
	(format t "~& ~5D ~A ~%" start (dec->min (* tempo (/ start 1000))))
	(incf start split)))

(defun average-hr (times hrs)
  (unless (member nil hrs)
    (/ (reduce #'+ (mapcar #'* times hrs))
       (reduce #'+ times))))

(defvar *maf-pace*
  '((13:20 10:00)
    (12:00 9:00)
    (10:40 8:00)
    (10:00 7:30)
    (9:00 7:00)
    (8:30 6:45)
    (8:00 6:30)
    (7:30 6:00)
    (7:00 5:30)
    (6:30 5:15)
    (6:00 5:00)
    (5:45 4:45)
    (5:30 4:30)
    (5:15 4:20)
    (5:00 4:15)))

(defun maf-pace (vo2max)
  (let ((5km-pace (* 1.609 (/ (vo2max->minutes vo2max 5000) 5))))
    (loop for ((x1 y1) (x2 y2)) on *maf-pace*
       when (>= y1 5km-pace (or y2 0)) do
	 (let ((dx (- x1 x2))
	       (dy (- y1 y2)))
	   (return-from maf-pace
	     (/ (+ (* (/ (- 5km-pace y2) dy)
		      dx)
		   x2)
		1.609))))))

#|
km/h	HR	BL
9	138	1.3
10	149	1.3
11	158	1.7
12	168	2.0
13	177	3.4
14	183	5.5
|#

#|
SB	21,1km	10,5km	10km	8,8km	7,5km	6,7km	2,7km	5000m	3000m	1500m	800m
2002	2:01:59
2003	2:11:40
2004	2:07:07	56:13		43:54
2005		56:22
2006	2:01:24					38:37
2007	1:59:11						11:47
2008	1:51:21		49:39		35:34		11:23		12:35	5:50	2:44
2009								22:30

GOAL:
2009	1:45		46:59		33:59				11:59	5:29	2:29	64.0	28.5	14.0
|#

(defvar *womens-records* '(("60 m"  	0:06.92)
			   ("100 m"  	0:10.49)
			   ("150 m"  	0:16.10)
			   ("200 m" 	0:21.34) ;21.87
			   ("300 m" 	0:35.30)
			   ("400 m" 	0:47.60) ;49.59
			   ("600 m" 	1:22.63)
			   ("800 m" 	1:53.28) ;1:55.87
			   ("1000 m" 	2:28.98) ;2:30.94
			   ("1500 m" 	3:50.46) ;3:58.28
			   ("1 mile" 	4:12.56) ;4:17.14
			   ("2000 m" 	5:25.36)
			   ("3000 m" 	8:06.11) ;8:23.72
			   ("2 miles"   8:58.58)
			   ("5000 m" 	14:11.15);14:24.37
			   ("10000 m" 	29:31.78)
			   ("10 km" 	30:21)
			   ("15 km" 	46:28)
			   ("10 miles"	50:01)
			   ("18517 m"	1:00:00)
			   ("20000 m" 	1:05:26.6)
			   ("20 km" 	1:02:21)
			   ("half marathon" 	1:05:50)
			   ("25000 m" 	1:27:05.9)
			   ("25 km" 	1:19:53)
			   ("30000 m" 	1:45:50.0)
			   ("30 km" 	1:36:36)
			   ("marathon" 	2:15:25)
			   ("100 km" 	6:33:11)))

(defvar *womens-milennium-records*
  '(("60 m"  		0:06.97)	;Jones-Ferrette, 2010
    ("100 m"  		0:10.64)	;Jeter, 2009
    ("200 m" 		0:21.69)	;Felix, 2012
    ("300 m" 		0:35.30)	;Guevara, 2003
    ("400 m" 		0:48.70)	;Richards-Ross, 2006
    ("600 m" 		1:22.87)	;Mutola, 2002
    ("800 m" 		1:54.01)	;Jelimo, 2008
    ("1000 m" 		2:30.12)	;Mutola, 2002
    ("1500 m" 		3:55.33)	;Ayhan-Lop, 2003
    ("1 mile" 		4:17.75)	;Jamal, 2007
    ("2000 m" 		5:30.19)	;Burka, 2009
    ("3000 m" 		8:21.42)	;Szabo, 2002
    ("2 miles" 		8:58.58)	;Defar, 2007
    ("5000 m" 		14:11.15)	;Dibaba, 2008
    ("10000 m" 		29:53.80)	;Melkamu, 2009
    ("15 km"	 	46:28)		;Dibaba, 2009
    ("10 miles"	 	50:01)		;Radcliffe, 2003
    ("20 km"	 	1:02:21)	;Radcliffe, 2003
    ("half marathon" 	1:05:50)	;Keitany, 2011
    ("30 km"	 	1:36:36)	;Radcliffe, 2003
    ("marathon" 	2:15:25)))	;Radcliffe, 2003

(defvar *swedish-womens-records*
  '(("100 m"	0:11.16)
    ("200 m"	0:22.82)
    ("400 m"	0:51.58)
    ("800 m"	1:59.44)
    ("1000 m"	2:38.70)
    ("1500 m"	4:05.49)
    ("1 mile"	4:25.34)
    ("2000 m"	5:52.22)
    ("3000 m"	8:48.87)
    ("5000 m"	15:06.90)
    ("10000 m"	31:57.15)
    ("half marathon"	1:11:04)
    ("marathon"	2:23:41)))

(defvar *my-records*			;VSM-standard (2009)
  '(("60 m"		 0:08.5)	;7.5
    ("100 m"		 0:13.06)	;13.0
    ("200 m"		 0:26.8)	;25.7
    ("300 m"		 0:43.42)
    ("400 m"		 0:59.74)	;56
    ("600 m"		 1:46.9)
    ("800 m"		 2:31.44)	;2:10
    ("1000 m"		 3:18.5)
    ("1500 m"		 5:25)		;4:30 
    ("2.661 km"		11:23)
    ("3000 m"		12:35)
    ("5000 m"		21:49)		;17:00
    ("7.5 km"		33:26)
    ("10 km"		49:39)
    ("half marathon"  1:42:44)))

(defvar *swedish-mens-records*
  '(("60 m"	0:6.58)
    ("100 m"	0:10.18)
    ("200 m"	0:20.30)
    ("400 m"	0:44.56)
    ("800 m"	1:45.45)
    ("1000 m"	2:17.8)
    ("1500 m"	3:36.49)
    ("1 mile"	3:54.45)
    ("2000 m"	5:02.09)
    ("3000 m"	7:42.24)
    ("5000 m"	13:17.59)
    ("10000 m"	27:55.74)
    ("10 km"	28:12)
    ("20000 m"	1:00:17.4)
    ("25000 m"	1:18:06.2)
    ("30000 m"	1:38:49.0)
    ("half marathon"  1:02:40)
    ("marathon"	2:10:38)
    ("100 km"	6:41:50)))

(defvar *mens-records* 
  '(("50 m"	0:05.56)
    ("60 m"	0:06.39)
    ("100 m"  	0:09.58)
    ("150 m"  	0:14.36)
    ("200 m" 	0:19.19)
   ;("250 m" 	0:25.24) ;Michael Johnson 300m split
                         ;Bolt 300m split 25.4A, Lemaitre 250m 26.25
    ("300 m"	0:30.85) ;Michael Johnson in altitude, Bolt 30.97
    ("400 m" 	0:43.18)
    ("600 m"	1:12.81)
    ("800 m" 	1:40.91) ;23.5 49.28 1:14.30
    ("1000 m" 	2:11.96)
    ("1500 m" 	3:26.00)
    ("1 mile" 	3:43.13)
    ("2000 m" 	4:44.79)
    ; 2500 m	6:01?
    ("3000 m" 	7:20.67)
    ("2 miles"	7:58.61)
    ; 4000 m    9:58?
    ;; 12:37.35 - 2:33.2, 2:32.2, 2:31.8, 2:30.5, 2:29.4
    ("5000 m" 	12:37.35)
    ; 7.5 km    19:23?
    ("10000 m" 	26:17.53)
    ("10 km" 	26:43.7)	;Leonard Patrick Komon, 2010
    ("15 km" 	41:12.5)	;Leonard Patrick Komon, 2010
    ("10 miles"	44:23.0)	;Haile Gebreselassie, 2005
    ("20000 m" 	56:26.0)
    ("20 km" 	55:21)		;Zerisenay Tadesse, 2010
    ("half marathon" 	58:23)	;Zerisenay Tadesse, 2010
    ("21285 m"	1:00:00)
    ("25000 m" 	1:12:25.0)	;Moses Mosop, 2011
    ("25 km" 	1:11:50)	;Samuel Kiplimo Kosgei, 2010
    ("30000 m" 	1:26:47.4)	;Moses Mosop, 2011
    ("30 km" 	1:27:44)	;Gebreselassie/Kosgei, 2009
    ("marathon"	2:03:23)	;Wilson Kipsang, 2013
    ("100 km" 	6:13:33)))

(defvar *mens-milennium-records*
  '(("60 m"  		0:06.39)	;Greene, 2001
    ("100 m"  		0:09.58)	;Bolt, 2009
    ("200 m" 		0:19.19)	;Bolt, 2009
    ("300 m" 		0:30.97)	;Bolt, 2010
    ("400 m" 		0:43.45)	;Wariner, 2007
    ("600 m" 		1:13.49)	;Mwengi, 2002
    ("800 m" 		1:41.01)	;Rudisha, 2010
    ("1000 m" 		2:13.62)	;Kaki, 2010
    ("1500 m" 		3:26.12)	;El Guerrouj, 2001
    ("1 mile" 		3:44.95)	;El Guerrouj, 2001
    ("2000 m" 		4:46.88)	;Saidi-Sief, 2001
    ("3000 m" 		7:25.02)	;Saidi-Sief, 2000
    ("2 miles" 		8:03.50)	;Mottram, 2007
    ("5000 m" 		12:37.35)	;Bekele, 2004
    ("10000 m" 		26:17.53)	;Bekele, 2005
    ("15 km" 		41:22)		;Gebrselassie, 2005
    ("10 miles"		44:24)		;Gebrselassie, 2005
    ("20 km" 		55:21)		;Tadese, 2010
    ("half marathon" 	58:23)		;Tadese, 2010
    ("21285 m" 		1:00:00)	;Gebrselassie, 2007
    ("30000 m" 		1:26:47.4)	;Mosop, 2011
    ("30 km" 		1:27:44)	;Gebrselassie, 2009
    ("marathon"		2:03:38)))	;Patrick Makau, 2011

(defvar *m40-records*
  '(("60 m"	0:06.78)
    ("100 m"  	0:10.29)
    ("200 m" 	0:20.64)
    ("400 m" 	0:47.81)
    ("800 m" 	1:50.34)
    ("1500 m" 	3:44.06)
    ("1 mile" 	4:02.53)
    ("3000 m" 	8:03.69)
    ("5000 m" 	13:43.15)
    ("10000 m" 	28:30.88)
    ("half marathon"	1:02:28)
    ("marathon"	2:08:46)))

(defvar *m50-records*
  '(("60 m"	0:07.18)
    ("100 m"  	0:10.95)
    ("200 m" 	0:22.53)
    ("400 m" 	0:51.39)
    ("800 m" 	1:58.65)
    ("1500 m" 	4:05.2)
    ("1 mile" 	4:27.9)
    ("3000 m" 	8:41.2)
    ("5000 m" 	14:53.2)
    ("10000 m" 	30:56.08)
    ("half marathon"	1:06:42)
    ("marathon"	2:19:29)))

(defvar *m60-records*
  '(("60 m"	0:07.66)
    ("100 m"  	0:11.70)
    ("200 m" 	0:24.00)
    ("400 m" 	0:53.88)
    ("800 m" 	2:10.42)
    ("1500 m" 	4:27.65)
    ("1 mile" 	4:54.07)
    ("3000 m" 	9:29.47)
    ("5000 m" 	16:12.57)
    ("10000 m" 	34:14.88)
    ("half marathon"	1:14:18)
    ("marathon"	2:36:30)))

(defvar *m70-records*
  '(("60 m"	0:08.20)
    ("100 m"  	0:12.77)
    ("200 m" 	0:26.48)
    ("400 m" 	0:59.34)
    ("800 m" 	2:20.52)
    ("1500 m" 	4:57.65)
    ("1 mile" 	5:23.58)
    ("3000 m" 	10:42.40)
    ("5000 m" 	18:33.38)
    ("10000 m" 	38:04.13)
    ("half marathon"	1:21:41)
    ("marathon"	2:54:48)))

#|
Brianmac:
200 time = 2.24 * 100 time
400 time = 5.14 * 100 time
800 time = 10.6 * 100 time
|#

#|
0-50m      9.0
60-50m	  12.1
60-100m   12.1
100-200m  10.4

Michael Johnson: 10.12+9.20
0-100m	   9.9
100-200m  10.8

Usain Bolt: 9.79+9.52
0-100m	  10.2
100-200m  10.5
|#

(defvar *x*
  '((200 	0:19.30)
    (300	0:30.85)
    (400 	0:43.18)
    (600	1:12.81)
    (800 	1:41.11)
    (1000 	2:11.96)
    (1500 	3:26.00)
    (1609.344 	3:43.13)
    (2000 	4:44.79)
    (3000 	7:20.67)
    (3218.688	7:58.61)
    (5000 	12:37.35)
    (10000 	26:17.53)
    (16093.44	44:24)
    (21097.5 	58:33)
    (25000 	1:12:45)
    (30000 	1:28:00)
    (42195	2:04:26)))

(defvar *x*
  '((  100 12.14)	;100->200: -0.96, *.92	100->1000: -4.48, *.63
    (  200 11.20)	;200->400: -1.61, *.86
    (  400  9.59)	;          -1.56, *.83
    (  800  8.03)	;          -0.77, *.90
    ( 1000  7.66)	;          -0.60, *.92	1000->10k: -1.32, *.82
    ( 1500  7.33)	;          -0.50, *.93
    ( 1600  7.26)
    ( 2000  7.06)       ;          -0.37, *.95
    ( 2500  6.93)	;          -0.32, *.95
    ( 3000  6.83)	;
    ( 4000  6.69)
    ( 5000  6.61)	;          -0.27, *.96
    (10000  6.34)	;          -0.43, *.93
    (20000  5.91)
    (21097  6.01)	;          -0.34, *.94
    (42195  5.67)))

(defvar *x*
  '(( 5000  6.61)
    (10000  6.34)
    (21097  6.01)
    (42195  5.67)))

(defvar *x*
  '(;;( 400 1.856)	;-1.25, *.32
    ( 800 0.601)	;-0.47, *.22
    (1000 0.329)	;-0.30, *.10
    (1500 0.177)
    (1600 0.135)
    (2000 0.034)))

#|
  100:  6.72  5.418  -0.75, *.86
  200:  6.53  4.672  -1.45, *.69
  400:  6.34  3.255  -1.34, *.59
  800:  6.14  1.888  -0.58, *.69
 1000:  6.08  1.580  -0.41, *.74
 1500:  5.97  1.363  -0.31, *.77
 1600:  5.95  1.311
 2000:  5.89  1.173  -0.18, *.85
 2500:  5.82  1.106  -0.13, *.89
 3000:  5.77  1.056
 4000:  5.69  0.997
 5000:  5.63  0.979  -0.08, *.92
10000:  5.44  0.902  -0.24, *.73
20000:  5.24  0.665
21097:  5.23  0.780  -0.15, *.81
42195:  5.04  0.633
|#

(defun heart-rate (&key (beats 10))
  (format t "Press enter to start.")
  (read-line)
  (let ((x (get-internal-real-time)))
    (format t "Press enter after ~R beats." beats)
    (read-line)
    (let ((y (/ (- (get-internal-real-time) x)
		internal-time-units-per-second))
	  (z (* 60 beats)))
      (let ((hr (/ z y))
	    (hr-lo (/ z (+ y 0.1)))
	    (hr-hi ( / z (- y 0.1))))
	(format t "~&Heart rate: ~,1F (~,1F-~,1F)~%" hr hr-lo hr-hi)))))

(defun continuous-heart-rate ()
  (format t "Press enter to start.")
  (read-line)
  (let ((list (list (get-internal-real-time))))
    (format t "Press enter every ten heart beat.")
    (loop
       (read-line)
       (push (get-internal-real-time) list)
       (setf list (subseq list 0 (min 41 (length list))))
       (loop for i from 1 to 10 do
	    (when (> (length list) i)
	      (format t "~D: ~,1F "
		      i
		      (/ (* 600 i)
			 (/ (- (nth 0 list) (nth i list))
			    internal-time-units-per-second))))))))

(defun aerobic-speed (x)
  ;;(* 12.27 (expt .951 (/ (log x) (log 2)))))
  ;;(- 11.06 (* .5110 (log x))))
  (- 10.37 (* .4399 (log x))))

(defun distance (x &optional (unit :km))
  (* (ecase unit
       (:km	1)
       ((:m :meters :metres)	1000)
       ((:mile :miles)		(/ 1.609344)))
     (multiple-value-bind (y z) (parse-integer x :junk-allowed t)
       (cond
	 ((numberp y)
	  (let ((z2 (string-downcase (string-trim " " (subseq x z)))))
	    (cond ((equalp z2 "m")	(* .001 y))
		  ((equalp z2 "km")	y)
		  ((equalp z2 "mile")	(* 1.609 y))
		  ((equalp z2 "miles")	(* 1.609 y)))))
	 ((equalp x "half marathon")
	  21.0975)
	 ((equalp x "marathon")
	  42.195)))))

(defun compare-records (list1 list2)
  (dolist (x list1)
    (let ((y (find (first x) list2 :key #'first :test #'equal)))
      (when y
	(format t "~&~A: time+~,1F%, or ~,1F% speed~%"
		(first x)
		(* 100 (1- (/ (second x) (second y))))
		(* 100 (/ (second y) (second x))))))))

(defun geometric-mean-time (time1 dist1 time2 dist2 dist3)
  "Computes the weighted geometric mean time for dist3, given
   time1 for dist1 and time2 for dist2."
  (let ((x (/ (- (log dist3) (log dist2)) (- (log dist1) (log dist2)))))
    (* (expt time1 x) (expt time2 (- 1 x)))))

(defmacro defworkout (date time &body workout)
  (unless (null time)
    (setq time (concatenate 'string "/" time)))
  (let ((name (concatenate 'string "*WORKOUT/" date time "*")))
    `(defparameter ,(intern name) ',workout)))

#|
(defworkout "2007-05-12" "14.49"
  (:work 5:04.3 1.0 :hr (165 :max 168))
  (:work 5:19.0 1.0 :hr (167 :max 171))
  (:work 5:27.8 1.0 :hr (166 :max 172))
  (:work 5:10.2 1.0 :hr (165 :max 169))
  (:work 5:37.3 1.0 :hr (169 :max 174))
  (:work 5:27.6 1.0 :hr (170 :max 175))
  (:work 4:57.1 1.0 :hr (164 :max 169))
  (:work 5:15.1 1.0 :hr (169 :max 172))
  (:work 5:17.6 1.0 :hr (169 :max 172))
  (:work 5:10.8 1.0 :hr (169 :max 172))
  (:work 5:14.9 1.0 :hr (170 :max 174))
  (:work 5:18.1 1.0 :hr (169 :max 171))
  (:work 5:19.1 1.0 :hr (171 :max 173))
  (:work 5:26.2 1.0 :hr (171 :max 175))
  (:work 5:34.4 1.0 :hr (168 :max 174))
  (:work 5:18.4 1.0 :hr (168 :max 174))
  (:work 5:26.3 1.0 :hr (171 :max 175))
  (:work 5:15.6 1.0 :hr (171 :max 175))
  (:work 5:13.4 1.0 :hr (174 :max 177))
  (:work 5:17.8 1.0 :hr (175 :max 178))
  (:work 5:10.3 1.1 :hr (178 :max 182)))|#

(defworkout "2008-01-06" "12.43"
  (:warm 10:43 1.23 :hr (127 :max 137))
  (:warm  9:17 1.23 :hr (140 :max 150))
  (:work 27:07 4.04 :hr (155 :max 162))
  (:work 26:29 4.02 :hr (157 :max 163))
  (:cool  8:23 1.17 :hr (151 :max 156)))

(defworkout "2008-01-10" "17.31"
  (:warm  8:44 1.29 :hr (138 :max 153))
  (:warm 16:16 2.62 :hr (154 :max 167))
  (:work 20:05 3.73 :hr (168 :max 172))
  (:cool 10:27 1.56 :hr (156 :max 171)))

(defworkout "2008-01-13" "12.10"
  (:warm 8:59 1.00 :hr (125 :max 129))
  (:warm 8:24 1.00 :hr (130 :max 136))
  (:warm 8:01 1.00 :hr (136 :max 139))
  (:work 7:46 1.00 :hr (141 :max 146))
  (:work 8:33 1.00 :hr (143 :max 148))
  (:work 6:42 1.00 :hr (142 :max 150))
  (:work 6:45 1.00 :hr (143 :max 146))
  (:work 7:04 1.00 :hr (145 :max 150))
  (:work 6:50 1.00 :hr (145 :max 149))
  (:work 6:45 1.00 :hr (144 :max 149))
  (:work 7:02 1.00 :hr (146 :max 152))
  (:work 6:50 1.00 :hr (146 :max 150))
  (:work 8:18 1.00 :hr (147 :max 152))
  (:work 5:02 0.71 :hr 147))

(defworkout "2008-01-17" "16.07"
  (:warm 15:01 2.42 :hr (148 :max 167))
  (:work  5:05 0.98 :hr (173 :max 177))
  (:rest  1:00 0.12 :hr (158 :max 171))
  (:work  5:05 0.99 :hr (170 :max 175))
  (:rest  1:00 0.13 :hr (161 :max 174))
  (:work  4:58 1.01 :hr (168 :max 175))
  (:rest  1:01 0.12 :hr (162 :max 173))
  (:work  5:01 1.00 :hr (167 :max 172))
  (:rest  1:01 0.14 :hr (162 :max 172))
  (:work  4:57 1.01 :hr (170 :max 174))
  (:cool 10:16 1.58 :hr (153 :max 173)))

(defworkout "2008-01-20" "10.38"
  (:warm 7:36 1.00 :hr (129 :max 142))
  (:warm 6:52 1.00 :hr (144 :max 150))
  (:warm 6:40 1.00 :hr (147 :max 153))
  (:work 6:50 1.00 :hr (148 :max 152))
  (:work 6:48 1.00 :hr (150 :max 154))
  (:work 6:48 1.00 :hr (149 :max 154))
  (:work 6:40 1.00 :hr (146 :max 151))
  (:work 6:42 1.00 :hr (146 :max 152))
  (:work 6:46 1.00 :hr (146 :max 154))
  (:work 5:36 1.00 :hr (160 :max 164))
  (:work 6:48 1.00 :hr (151 :max 164))
  (:work 6:56 1.00 :hr (146 :max 151))
  (:work 0:36 0.09 :hr 147))

(defworkout "2008-01-21" "10.30"
  (:work 32:02 5.12 :hr (151 :max 177)))

(defworkout "2008-01-24" "16.27"
  (:warm 20:34 3.50 :hr (152 :max 171))
  (:work 19:51 4.01 :hr (171 :max 173))
  (:cool  5:03 0.75 :hr (154 :max 172)))

(defworkout "2008-01-26" "12.37"
  (:warm 8:26 1.0 :hr (132 :max 140))
  (:warm 8:14 1.0 :hr (135 :max 144))
  (:work 7:22 1.0 :hr (140 :max 145))
  (:work 7:26 1.0 :hr (140 :max 145))
  (:work 6:52 1.0 :hr (143 :max 148))
  (:work 6:54 1.0 :hr (141 :max 148))
  (:work 7:20 1.0 :hr (141 :max 150))
  (:work 6:56 1.0 :hr (144 :max 149))
  (:work 7:10 1.0 :hr (143 :max 149))
  (:work 8:00 1.0 :hr (143 :max 150))
  (:work 7:26 1.0 :hr (144 :max 152))
  (:work 6:52 1.0 :hr (144 :max 148))
  (:work 7:06 1.0 :hr (144 :max 150))
  (:work 7:22 1.0 :hr (144 :max 152))
  (:work 7:24 1.0 :hr (144 :max 152))
  (:work 1:02 .16 :hr (144 :max 150)))

(defworkout "2008-01-28" "11.30"
  (:work 37:01 5.99 :hr (149 :max 179)))

(defworkout "2008-01-31" "16.23"
  (:warm 16:00.2 2.69 :hr (161 :max 172))
  (:work  5:07.7 1.00 :hr (172 :max 176))
  (:rest  0:45.5 0)
  (:work  5:15.8 1.00 :hr (168 :max 175))
  (:rest  0:45.5 0)
  (:work  5:15.5 1.00 :hr (169 :max 174))
  (:rest  0:45.8 0)
  (:work  5:15.5 1.00 :hr (170 :max 175))
  (:rest  0:45.7 0)
  (:work  5:15.5 1.00 :hr (171 :max 177))
  (:cool  8:00.9 1.07 :hr (156 :max 177)))

(defworkout "2008-02-03" "11.34"
  (:warm 7:00 1.0 :hr (138 :max 148))
  (:warm 6:48 1.0 :hr (148 :max 153))
  (:work 6:52 1.0 :hr (153 :max 159))
  (:work 6:58 1.0 :hr (153 :max 159))
  (:work 7:50 1.0 :hr (158 :max 162))
  (:work 5:50 1.0 :hr (152 :max 157))
  (:work 6:34 1.0 :hr (150 :max 153))
  (:work 6:24 1.0 :hr (151 :max 154))
  (:work 6:24 1.0 :hr (153 :max 157))
  (:work 6:28 1.0 :hr (151 :max 155))
  (:work 5:32 1.0 :hr (160 :max 164))
  (:work 5:44 1.0 :hr (165 :max 171))
  (:work 6:34 1.0 :hr (161 :max 174))
  (:work 6:48 1.0 :hr (152 :max 155))
  (:cool 0:31 .07 :hr 150))

(defworkout "2008-02-04" "10.53"
  (:warm  6:36 1.00 :hr (133 :max 142))
  (:warm  6:12 1.00 :hr (145 :max 150))
  (:warm  6:20 1.00 :hr (147 :max 151))
  (:work 17:01 3.08 :hr (155 :max 172)))

(defworkout "2008-02-09" "10.54"
  (:warm 21:00.7 3.43 :hr (144 :max 165))
  (:work  5:05.9 1.01 :hr (169 :max 174))
  (:rest  0:31.2 0.08 :hr (172 :min 168))
  (:work  5:05.8 1.00 :hr (170 :max 172))
  (:rest  0:30.9 0.08 :hr (168 :min 163))
  (:work  5:05.7 1.00 :hr (168 :max 171))
  (:rest  0:30.9 0.07 :hr (167 :min 163))
  (:work  5:05.6 1.00 :hr (166 :max 171))
  (:rest  0:30.8 0.07 :hr (168 :min 165))
  (:work  5:16.1 1.02 :hr (166 :max 171))
  (:cool 16:29.5 2.73 :hr 155))

(defworkout "2008-02-10" "11.12"
  (:warm 1:56 3174/10)
  (:work 1:28 3174/10)
  (:work 2:22 400))

(defworkout "2008-02-11" "11.02"
  (:warm 6:37.9 1.0 :hr (134 :max 141))
  (:warm 6:31.9 1.0 :hr (142 :max 146))
  (:work 6:24.0 1.0 :hr (147 :max 153))
  (:work 6:28.0 1.0 :hr (146 :max 152))
  (:work 6:12.0 1.0 :hr (148 :max 153))
  (:cool 0:23   .05 :hr 148))

(defworkout "2008-02-14" "10.17"
  (:warm  8:13.6 1.32 :hr (142 :max 158))
  (:warm  0:24.8 0.09 :hr (159 :max 163))
  (:warm  7:07.6 1.18 :hr (152 :max 166))
  (:warm  0:22.0 0.09 :hr (167 :max 170))
  (:warm  2:29.9 0.52 :hr (165 :max 172))
  (:work 10:20.6 2.03 :hr (168 :max 171))
  (:work 10:01.3 1.97 :hr (169 :max 174))
  (:cool 10:55.6 1.81 :hr (157 :max 172)))

(defworkout "2008-02-16" "11.18"
  (:warm 8:24.1 1.3 :hr (137 :max 144))
  (:work 6:20.1 1.0 :hr (143 :max 148))
  (:work 6:28.0 1.0 :hr (143 :max 145))
  (:work 6:44.0 1.0 :hr (144 :max 148))
  (:work 7:57.9 1.0 :hr (146 :max 150))
  (:work 5:42.1 1.0 :hr (143 :max 149))
  (:work 6:05.9 1.0 :hr (146 :max 148))
  (:work 6:18.1 1.0 :hr (147 :max 152))
  (:work 6:10.3 1.0 :hr (141 :max 149))
  (:work 6:09.8 1.0 :hr (147 :max 154))
  (:work 6:14.1 1.0 :hr (147 :max 153))
  (:work 6:18.0 1.0 :hr (146 :max 150))
  (:work 6:03.9 1.0 :hr (146 :max 149))
  (:work 6:12.0 1.0 :hr (148 :max 151))
  (:work 6:06.0 1.0 :hr (148 :max 151))
  (:work 6:52.0 1.0 :hr (150 :max 155))
  (:work 6:32.0 1.0 :hr (145 :max 148))
  (:work 0:12   .02 :hr 145))

;;;(defworkout "2008-02-19" ""

;;;(defworkout "2008-02-24" ""

;;;(defworkout "2008-02-25" ""

;;;(defworkout "2008-02-26" ""

;;;(defworkout "2008-02-28" ""

;;;(defworkout "2008-03-02" ""

;;;(defworkout "2008-03-03" ""

;;;(defworkout "2008-03-04" ""

;;;(defworkout "2008-03-06" ""

;;;(defworkout "2008-03-09" ""

;;;(defworkout "2008-03-13" ""

;;;(defworkout "2008-03-16" ""

;;;(defworkout "2008-03-17" ""

;;;(defworkout "2008-03-18" ""

;;;(defworkout "2008-03-20" ""

;;;(defworkout "2008-03-23" ""

;;;(defworkout "2008-03-27" ""

;;;(defworkout "2008-03-29" ""

;;;(defworkout "2008-03-31" ""

;;;(defworkout "2008-04-01" ""

;;;(defworkout "2008-04-03" ""

;;;(defworkout "2008-04-06" ""

;;;(defworkout "2008-04-07" ""

;;;(defworkout "2008-04-10" ""

;;;(defworkout "2008-04-12" ""

;;;(defworkout "2008-04-13" ""

;;;(defworkout "2008-04-14" ""

;;;(defworkout "2008-04-15" ""

;;;(defworkout "2008-04-17" ""

(defworkout "2008-04-19" "11.00"
  (:work 2:31.9 0.5 :hr (167 :max 172))
  (:work 2:21.9 0.5 :hr (174 :max 177))
  (:work 2:22.0 0.5 :hr (174 :max 177))
  (:work 2:34.1 0.5 :hr (175 :max 176))
  (:work 2:29.9 0.5 :hr (175 :max 176))
  (:work 2:34.2 0.5 :hr (176 :max 178))
  (:work 2:15.9 0.5 :hr (176 :max 178))
  (:work 2:21.9 0.5 :hr (176 :max 178))
  (:work 2:24.3 0.5 :hr (176 :max 179))
  (:work 2:25.8 0.5 :hr (175 :max 176))
  (:work 2:22.1 0.5 :hr (176 :max 177))
  (:work 2:25.9 0.5 :hr (175 :max 178))
  (:work 2:18.0 0.5 :hr (178 :max 179))
  (:work 2:21.9 0.5 :hr (179 :max 180))
  (:work 2:05.7 0.5 :hr (182 :max 186)))

;;;(defworkout "2008-04-21" ""

;;;(defworkout "2008-04-24" ""

;;;(defworkout "2008-04-27" ""

;;;(defworkout "2008-04-28" ""

;;;(defworkout "2008-04-29" ""

;;;(defworkout "2008-05-01" ""

;;;(defworkout "2008-05-04" ""

;;;(defworkout "2008-05-05" ""

;;;(defworkout "2008-05-07" ""

;;;(defworkout "2008-05-10" ""

;;;(defworkout "2008-05-10" ""

;;;(defworkout "2008-05-12" ""

;;;(defworkout "2008-05-15" ""

;;; 26:47  00:53:03  01:19:41  01:46:11
(defworkout "2008-05-17" "14.49"
  (:work 5:04.3 1.0 :hr (165 :max 168))
  (:work 5:19.0 1.0 :hr (167 :max 171))
  (:work 5:27.8 1.0 :hr (166 :max 172))
  (:work 5:10.2 1.0 :hr (165 :max 169))
  (:work 5:45.7 1.0 :hr (169 :max 174)) ;5:37.3
  (:work 5:19.2 1.0 :hr (170 :max 175)) ;5:27.6
  (:work 5:07.0 1.0 :hr (164 :max 169)) ;4:57.1
  (:work 5:15.1 1.0 :hr (169 :max 172))
  (:work 5:17.6 1.0 :hr (169 :max 172))
  (:work 5:17.1 1.0 :hr (169 :max 172)) ;5:10.8
  (:work 5:14.9 1.0 :hr (170 :max 174))
  (:work 5:18.1 1.0 :hr (169 :max 171))
  (:work 5:19.1 1.0 :hr (171 :max 173))
  (:work 5:26.2 1.0 :hr (171 :max 175))
  (:work 5:19.7 1.0 :hr (168 :max 174)) ;5:34.4
  (:work 5:18.4 1.0 :hr (168 :max 174)) ;5:18.4
  (:work 5:26.3 1.0 :hr (171 :max 175))
  (:work 5:15.6 1.0 :hr (171 :max 175))
  (:work 5:13.4 1.0 :hr (174 :max 177))
  (:work 5:16.3 1.0 :hr (175 :max 178)) ;5:17.5
  (:work 5:10.3 1.1 :hr (178 :max 182)))

(defworkout "2008-05-21" "19.31"
  (:work 1:09.2 300 :hr (171 :max 181))
  (:work 1:39.0 400 :hr (185 :max 186))
  (:work 1:40.5 400 :hr (187 :max 190))
  (:work 1:36.8 400 :hr (190 :max 191)))

;;;(defworkout "2008-05-21" ""

;;;(defworkout "2008-05-22" ""

;;;(defworkout "2008-05-25" ""

;;;(defworkout "2008-05-26" ""

(defworkout "2008-05-27" "19.31"
  (:work 0:48.2 200 :hr (158 :max 171))
  (:work 1:47.5 400 :hr (176 :max 181))
  (:work 1:47.6 400 :hr (181 :max 183))
  (:work 1:44.5 400 :hr (183 :max 185))
  (:work 1:45.1 400 :hr (185 :max 187))
  (:work 1:46.0 400 :hr (187 :max 188))
  (:work 1:45.9 400 :hr (187 :max 188))
  (:work 1:39.6 400 :hr (184 :max 189)))

;;;(defworkout "2008-05-27" ""

;;;(defworkout "2008-05-30" ""

;;;(defworkout "2008-05-31" ""

(defworkout "2008-06-01" "19.39"
  (:work 6:07.8 1.0 :hr (139 :max 147))
  (:work 6:04.1 1.0 :hr (145 :max 149))
  (:work 6:06.0 1.0 :hr (147 :max 149))
  (:work 6:14.1 1.0 :hr (147 :max 156))
  (:work 6:52.0 1.0 :hr (154 :max 160))
  (:work 6:20.2 1.0 :hr (150 :max 157))
  (:work 5:49.9 1.0 :hr (145 :max 152))
  (:work 6:02.1 1.0 :hr (150 :max 154))
  (:work 6:00.0 1.0 :hr (150 :max 156))
  (:work 5:56.0 1.0 :hr (150 :max 155))
  (:work 6:00.0 1.0 :hr (150 :max 152))
  (:work 5:58.1 1.0 :hr (151 :max 153))
  (:work 6:16.9 1.0 :hr (151 :max 156))
  (:work 5:59.9 1.0 :hr (152 :max 158))
  (:work 3:38.7 0.58 :hr (154 :max 157)))

(defworkout "2008-06-09" "11.11"
  (:work 6:15.9 1.0)
  (:work 6:06.0 1.0)
  (:work 6:04.0 1.0)
  (:work 5:56.0 1.0)
  (:work 5:36.0 1.0)
  (:work 5:50.3 1.0)
  (:work 2:33.3 0.43))

(defworkout "2008-06-12" "17.04"
  (:warm 14:10.2 2.44)
  (:work 0:59.9 300)
  (:rest 3:10.8)
  (:work 0:59.3 300)
  (:rest 3:17.6)
  (:work 1:08.3 300)
  (:rest 3:09.7)
  (:work 1:05.0 300)
  (:rest 3:33.8)
  (:work 1:08.6 300)
  (:rest 2:57.5)
  (:work 1:05.9 300)
  (:rest 3:20.4)
  (:work 1:05.9 300)
  (:rest 4:03.6)
  (:work 0:55.5 300)
  (:rest 3:55.9)
  (:work 1:02.1 300)
  (:rest 4:20.4)
  (:cool 6:56.2 1.25))

(defworkout "2008-06-15" "11.28"
  (:work 6:15.8 1.0)
  (:work 6:08.0 1.0)
  (:work 6:35.6 1.08)
  (:work 6:41.0 1.10)
  (:work 6:02.0 1.0)
  (:work 6:20.0 1.0)
  (:work 6:12.4 1.0)
  (:work 6:16.0 1.0)
  (:work 5:55.9 1.0)
  (:work 5:46.0 1.0)
  (:work 5:46.0 1.0)
  (:work 6:04.0 1.0)
  (:work 6:00.0 1.0)
  (:work 5:30.1 1.0)
  (:work 4:18.6 0.68))

(defworkout "2008-06-16" "11.08"
  (:warm 6:41.8 1.0)
  (:work 6:20.3 1.0)
  (:work 5:58.0 1.0)
  (:work 6:06.0 1.0)
  (:work 5:32.0 1.0)
  (:work 5:20.0 1.0)
  (:work 5:26.0 1.0)
  (:work 0:55.1 0.15))

(defworkout "2008-06-19" "16.28"
  (:warm 14:53.1 2500)
  (:work  0:42.9  200)
  (:rest  2:32.4    0)
  (:work  0:40.8  200)
  (:rest  2:19.5    0)
  (:work  0:38.8  200)
  (:rest  2:26.2    0)
  (:work  0:38.0  200)
  (:rest  2:34.7    0)
  (:work  0:37.4  200)
  (:rest  2:21.0    0)
  (:work  0:36.5  200)
  (:rest  2:28.4    0)
  (:work  0:39.5  200)
  (:rest  2:26.2    0)
  (:work  0:37.4  200)
  (:rest  2:34.9    0)
  (:work  0:37.9  200)
  (:rest  2:33.9    0)
  (:work  0:38.0  200)
  (:rest  3:12.7    0)
  (:work  0:37.7  200)
  (:rest  2:40.6    0)
  (:work  0:35.3  200)
  (:rest  4:19.1    0)
  (:work  0:32.3  200)
  (:rest  5:05.1    0)
  (:work  0:31.4  200)
  (:rest  2:20.0    0)
  (:cool  6:50.3 1200))

(defworkout "2008-06-21" "9.25"
  (:work 6:13.9 1.0 :hr (131 :max ?))
  (:work 6:14.2 1.0 :hr (146 :max 152))
  (:work 6:17.6 1.0 :hr (149 :max 152))
  (:work 6:26.0 1.0 :hr (149 :max 156))
  (:work 6:30.6 1.0 :hr (152 :max 156))
  (:work 6:08.0 1.0 :hr (149 :max 155))
  (:work 5:52.0 1.0 :hr (149 :max 152))
  (:work 6:04.1 1.0 :hr (151 :max 154))
  (:work 5:55.8 1.0 :hr (149 :max 154))
  (:work 6:02.0 1.0 :hr (149 :max 152))
  (:work 5:54.0 1.0 :hr (150 :max 153))
  (:work 0:37.0 0.19 :hr (151 :max 153)))

(defworkout "2008-06-22" "12.11"
  (:warm  9:15.3 1.6  :hr (144 :max 158))
  (:warm  6:03.6 1.2  :hr (158 :max 172))
  (:work  7:00.2 1500 :hr (168 :max 175))
  (:rest  0:32.9 0    :hr (173 :min 170))
  (:work  6:54.5 1500 :hr (176 :max 180))
  (:rest  0:57.8 0    :hr (172 :min 161))
  (:work  6:53.8 1500 :hr (176 :max 180))
  (:rest  1:03.9 0    :hr (166 :min 159))
  (:work  6:45.2 1500 :hr (177 :max 183))
  (:cool 10:57.0 2.0  :hr (160 :max 181)))

(defworkout "2008-06-23" "11.48"
  (:work 6:15.9 1.0)
  (:work 6:00.2 1.0)
  (:work 6:05.8 1.0)
  (:work 5:53.9 1.0)
  (:work 5:46.2 1.0)
  (:work 5:46.1 1.0)
  (:work 5:54.0 1.0)
  (:work 6:08.7 1.0)
  (:work 5:03.1 0.85))

(defworkout "2008-06-26" "15.36"
  (:warm 9:09.2 1600)
  (:warm 8:17.9 1600)
  (:work 1:09.5 300)
  (:rest 2:52.9)
  (:work 1:05.3 300)
  (:rest 2:57.3)
  (:work 1:06.2 300)
  (:rest 3:15.2)
  (:work 1:05.4 300)
  (:rest 3:12.7)
  (:work 1:05.6 300)
  (:rest 2:55.9)
  (:work 1:06.0 300)
  (:rest 2:55.8)
  (:work 1:03.2 300)
  (:rest 2:59.6)
  (:work 0:56.3 300)
  (:rest 3:11.8)
  (:work 1:04.9 300)
  (:rest 3:16.1)
  (:work 1:04.2 300)
  ;(:rest 4:41.9)
  (:cool 6:01.5 1200)
  (:cool 8:15.7 1380))

(defworkout "2008-06-29" "10.33"
  (:work 6:57.2 1.1 :hr 140)
  (:work 5:41.8 0.92 :hr (151 :max 153))
  (:work 7:49.3 1.27 :hr (150 :max 154))
  (:work 7:49.7 1.27 :hr (151 :max 158))
  (:work 6:56.2 1.0 :hr (155 :max 160))
  (:work 5:43.9 1.0 :hr (147 :max 152))
  (:work 6:12.1 1.0 :hr (152 :max 159))
  (:work 6:16.0 1.0 :hr (152 :max 159))
  (:work 6:04.0 1.0 :hr (149 :max 151))
  (:work 6:10.0 1.0 :hr (151 :max 156))
  (:work 6:06.0 1.0 :hr (151 :max 154))
  (:work 6:04.0 1.0 :hr (151 :max 156))
  (:work 6:09.9 1.0 :hr (151 :max 156))
  (:work 6:00.1 1.0 :hr (154 :max 159))
  (:work 5:50.0 1.0 :hr (165 :max 172))
  (:work 5:54.0 1.0 :hr (154 :max 168))
  (:work 1:25.2 0.21 :hr (153 :max 155)))

(defworkout "2008-06-30" "10.55"
  (:work 7:10.2 1.1)
  (:work 6:14.0 1.0)
  (:work 6:45.4 1.1)
  (:work 6:02.0 1.0)
  (:work 5:54.0 1.0)
  (:work 3:50.4 0.64)
  (:work 6:02.0 1.0)
  (:work 6:01.8 1.0)
  (:work 5:29.2 0.93))

(defworkout "2008-07-03" "16.37"
    (:warm 6:55.3 1200)
    (:warm 8:07.0 1600)
    (:warm 1:05.5  200)
    (:work 4:09.4 1000 :hr (nil :max 182))
    (:rest 3:00.0  100 :hr (nil :min 148))
    (:work 4:23.4 1000 :hr (nil :max 183))
    (:rest 2:59.7  100 :hr (nil :min 142))
    (:work 4:14.3 1000 :hr (177 :max 188))
    (:rest 3:03.6  100 :hr (164 :min 141))
    (:work 4:19.4 1000 :hr (177 :max 190))
   ;(:rest 4:09.5    0 :hr (151 :min 134))
    (:cool 6:23.6 1200 :hr (167 :max 177))
    (:cool 7:10.7 1200 :hr (168 :max 175)))

(defworkout "2008-07-10" "15.04"
    (:warm 9:16.2 1500 :hr 140)
   ;(:pause 7:38.6)
    (:warm 7:49.6 1500 :hr 156)
    (:work 0:49.6 200 :hr (152 :max 166))
    (:work 0:55.0 200 :hr (169 :max 171))
    (:work 2:35.6 600 :hr (175 :max 177))
    (:rest 3:00     0 :hr (143 :min 142)) ; 4:41.9
    (:work 0:52.3 200 :hr (154 :max 166))
    (:work 0:52.3 200 :hr (172 :max 173))
    (:work 0:51.0 200 :hr (175 :max 178))
    (:work 0:49.5 200 :hr (179 :max 180))
    (:work 0:50.7 200 :hr (180 :max 181))
    (:rest 2:59.9   0 :hr (149 :min 129))
    (:work 0:49.7 200 :hr (149 :max 166))
    (:work 0:53.8 200 :hr (170 :max 172))
    (:work 0:50.0 200 :hr (177 :max 179))
    (:work 0:49.9 200 :hr (180 :max 181))
    (:work 0:48.8 200 :hr (180 :max 181))
    (:rest 2:55.9   0 :hr (149 :min 125))
    (:work 0:48.8 200 :hr (151 :max 167))
    (:work 0:51.2 200 :hr (174 :max 177))
    (:work 0:48.9 200 :hr (179 :max 181))
    (:work 0:48.1 200 :hr (182 :max 184))
    (:work 0:48.3 200 :hr (184 :max 186))
  #|(:rest 2:29.3   0 :hr (143 :min 124))
    (:rest 2:22.3   0 :hr (144 :max 166))|#)

(defworkout "2008-07-10" "16.09"
    (:work 0:41.8  200 :hr (154 :max 170))
    (:rest 1:55.8    0 :hr (141 :min 124))
    (:work 0:42.5  200 :hr (146 :max 172))
    (:rest 2:00.2    0 :hr (138 :min 118))
    (:work 0:39.0  200 :hr (151 :max 173))
    (:rest 1:56.1    0 :hr (144 :min 122))
    (:work 0:38.5  200 :hr (145 :max 175))
    (:rest 2:00.5    0 :hr (146 :min 123))
    (:work 0:35.0  200 :hr (150 :max 177))
    (:rest 2:10.4    0 :hr (150 :min 132))
    (:work 0:32.6  200 :hr (156 :max 178))
    (:cool 5:21.1 1000 :hr (154 :max 159))
    (:cool 5:59.6 1000 :hr (146 :max 151)))

(defworkout "2008-07-12" "11.11"
    (:warm 6:39.8 1.00 :hr (139 :max 147))
    (:warm 2:04.7 0.32 :hr (148 :max 153))
    (:work 6:19.7 1.00 :hr (145 :max 153))
    (:work 6:22.0 1.00 :hr (152 :max 155))
    (:work 6:20.3 1.00 :hr (152 :max 155))
    (:work 6:35.7 1.00 :hr (151 :max 154))
    (:work 6:14.1 1.00 :hr (153 :max 155))
    (:work 6:26.0 1.00 :hr (156 :max 159))
    (:work 6:00.1 1.00 :hr (156 :max 160))
    (:work 6:05.8 1.00 :hr (159 :max 161))
    (:cool 6:28.1 1.00 :hr (155 :max 162))
    (:cool 0:35.1 0.08 :hr (149 :max 151)))

(defworkout "2008-07-13" "19.00"
    (:warm 8:25.5  1500 :hr (150 :max 160))
   ;(:pause 4:32.3      :hr (124 :min 120))
    (:warm 7:32.9  1500 :hr (165 :max 176))
   ;(:pause 3:29.7      :hr (140 :min 133))
    (:work 0:21.5    80 :hr (143 :max 144)) ;1
    (:work 0:46.9 .1587 :hr (157 :max 164))

    (:work 0:46.9 .1587 :hr (167 :max 169))
    (:work 0:43.3 .1587 :hr (170 :max 173))
    (:work 0:45.4 .1587 :hr (175 :max 175)) ;5
    (:work 0:44.0 .1587 :hr (175 :max 176))
    (:work 0:44.4 .1587 :hr (176 :max 177))

    (:work 0:44.9 .1587 :hr (176 :max 178))
    (:work 0:45.3 .1587 :hr (176 :max 177))
    (:work 0:45.3 .1587 :hr (175 :max 175)) ;10
    (:work 0:45.6 .1587 :hr (176 :max 177))
    (:work 0:45.2 .1587 :hr (175 :max 176))
    (:work 0:44.9 .1587 :hr (177 :max 177))

    (:work 0:45.0 .1587 :hr (177 :max 177))
    (:work 0:44.7 .1587 :hr (177 :max 178)) ;15
    (:work 0:44.4 .1587 :hr (176 :max 178))
    (:work 1:28.3 .3174 :hr (178 :max 179))
    (:work 0:44.1 .1587 :hr (179 :max 180))

    (:work 0:43.5 .1587 :hr (181 :max 181))
    (:work 0:43.4 .1587 :hr (181 :max 181)) ;20
    (:work 0:44.0 .1587 :hr (181 :max 181))
    (:work 0:43.8 .1587 :hr (181 :max 182))
    (:work 0:44.6 .1587 :hr (181 :max 182))
    (:work 0:43.5 .1587 :hr (182 :max 182))

    (:work 0:44.3 .1587 :hr (182 :max 183)) ;25
    (:work 0:43.6 .1587 :hr (181 :max 182))
    (:work 0:44.4 .1587 :hr (182 :max 182))
    (:work 0:43.7 .1587 :hr (180 :max 182))
    (:work 0:43.4 .1587 :hr (183 :max 184))
    (:work 0:40.1 .1587 :hr (185 :max 186)) ;30
    (:work 0:39.5 .1587 :hr (185 :max 186))
   ;(:pause 5:00.8      :hr (140 :min 127))
    (:cool 5:25.0  1000 :hr (155 :max 162))
   ;(:pause 2:36.3      :hr (133 :min 120))
    (:cool 6:01.3  1000 :hr (144 :max 150)))

(defworkout "2008-07-17" "13.41"
    (:warm 9:02.8 1500 :hr (147 :max 155))
    (:warm 7:50.3 1500 :hr (163 :max 175))
    (:work 0:51.1  200 :hr (164 :max 175))
    (:work 0:49.3  200 :hr (179 :max 181))
    (:work 0:52.6  200 :hr (180 :max 181))
    (:work 0:50.9  200 :hr (182 :max 184))
    (:work 0:51.2  200 :hr (184 :max 185))
    (:rest 3:07.1    0 :hr (151 :min 144))
    (:work 0:52.7  200 :hr (165 :max 173))
    (:work 0:52.2  200 :hr (177 :max 180))
    (:work 0:51.9  200 :hr (182 :max 184))
    (:work 0:52.3  200 :hr (182 :max 184))
    (:work 0:49.7  200 :hr (184 :max 185))
    (:rest 3:29.7    0 :hr (151 :min 143))
    (:work 0:49.0  200 :hr (160 :max 170))
    (:work 0:51.9  200 :hr (178 :max 181))
    (:work 0:50.6  200 :hr (183 :max 184))
    (:work 0:50.1  200 :hr (186 :max 186))
    (:work 0:49.6  200 :hr (186 :max 187))
    (:rest 2:56.8    0 :hr (151 :min 142))
    (:work 0:41.4  164 :hr (159 :max 169))
    (:work 0:09.1   36 :hr (171 :max 171))
    (:work 0:52.0  200 :hr (178 :max 182))
    (:work 0:50.0  200 :hr (185 :max 186))
    (:work 0:49.1  200 :hr (187 :max 187))
    (:work 0:50.2  200 :hr (187 :max 188))
    (:rest 3:11.2    0 :hr (153 :min 142))
    (:work 0:51.6  200 :hr (166 :max 174))
    (:work 0:54.4  210 :hr (180 :max 183))
    (:work 0:47.1  190 :hr (184 :max 185))
    (:work 0:48.9  200 :hr (187 :max 187))
    (:work 0:47.9  200 :hr (187 :max 187))
  #|(:pause 8:59.1   0 :hr (141 :min 130))|#)

(defworkout "2008-07-17" "14.52"
    (:work 0:59.7  300 :hr (169 :max 181))
    (:rest 5:53.6    0 :hr (137 :min 135))
    (:work 0:58.9  300 :hr (167 :max 179))
    (:rest 4:04.4    0 :hr (138 :min 124))
    (:work 1:02.6  300 :hr (160 :max 178))
    (:rest 3:16.7    0 :hr (141 :min 124))
    (:work 0:57.7  300 :hr (164 :max 182))
  #|(:pause 4:47     0 :hr (136 :min 123))|#
    (:cool 5:05.5 1000 :hr (161 :max 169))
   ;(:rest 1:21.2    0 :hr (151 :min 134))
    (:cool 5:58.0 1000 :hr (152 :max 156)))

(defworkout "2008-07-19" "11.39"
    (:work 7:41.8 1.0)
    (:work 6:58.0 1.0)
    (:work 7:29.3 1.1)
    (:work 6:50.2 1.0)
    (:work 6:03.9 0.90)
   ;(:work 1:07.8 0.02)
    (:work 0:36.2 0.08)
    (:work 7:14.2 1.1)
   ;(:work 0:20.3 0.03)
   ;(:work 0:57.5 0.05)
    (:work 6:30.2 0.92)
    (:work 6:16.0 1.0)
    (:work 6:20.1 1.0)
    (:work 7:02.7 1.1)
    (:work 6:04.1 1.0)
    (:work 4:11.8 0.63)
    (:work 2:48.3 0.47)
    (:work 6:32.1 1.0)
    (:work 6:35.8 1.0)
    (:work 6:28.0 1.0)
    (:work 2:59.7 0.48))

(defworkout "2008-07-21" "17.18"
    (:work 5:59.8 1.0)
    (:work 5:54.0 1.0)
    (:work 5:52.0 1.0)
    (:work 5:38.0 1.0)
    (:work 5:50.0 1.0)
    (:work 5:46.0 1.0)
    (:work 5:32.0 1.0)
    (:work 5:32.0 1.0)
    (:work 5:40.0 1.0)
    (:work 5:48.0 1.0)
    (:work 6:30.2 1.05)
    (:work 3:26.0 0.56))

(defworkout "2008-07-24" "13.17"
    (:warm 7:15.2 1200)
   ;(:pause 5:07.8)
    (:warm 8:17.9 1600)
    (:work 1:48.7 400)
    (:rest 0:59.1)
    (:work 1:45.6 400)
    (:rest 1:00.6)
    (:work 1:46.0 400)
    (:rest 1:01.5)
    (:work 1:44.0 400)
    (:rest 1:04.4)
    (:work 1:44.2 400)
    (:rest 0:58.6)
    (:work 1:41.5 400)
    (:rest 1:01.8)
    (:work 1:41.9 400)
    (:rest 0:59.9)
    (:work 1:40.9 400)
    (:rest 1:04.9)
    (:work 1:39.4 400)
    (:rest 1:09.0)
    (:work 1:38.4 400)
    (:rest 1:00.5)
    (:work 1:34.9 400)
    (:rest 1:02.3)
    (:work 1:29.6 400)
  #|(:pause 21:00)|#)

(defworkout "2008-07-24" "14.44"
    (:work 1:34.0 400)
    (:rest 0:40.0)
    (:work 1:36.9 400)
    (:rest 0:42.8)
    (:work 1:35.4 400)
    (:rest 0:35.2)
    (:work 1:26.2 400)
   ;(:pause 8:00)
    (:cool 8:20.8 1.3))

(defworkout "2008-07-26" "20.47"
    (:work 6:17.9 1.0)
    (:work 6:07.9 1.0)
    (:work 6:06.0 1.0)
    (:work 6:02.0 1.0)
    (:work 0:14.7 0.04)
    (:work 5:43.8 1.0)
    (:work 6:02.2 1.0)
    (:work 6:01.8 1.0)
    (:work 6:00.1 1.0)
    (:work 6:10.1 1.0)
    (:work 1:02.9 0.17))

(defworkout "2008-07-27" "09.45"
    (:warm 7:30  1200 :hr (145 :max 151))
    (:warm 6:40  1200 :hr (165 :max 170))
    (:work 1:59.5 400 :hr (152 :max 164))
    (:work 1:54.2 400 :hr (167 :max 171))
    (:work 1:54.7 400 :hr (172 :max 173))
    (:rest 0:40.5   0 :hr (169 :min 158))
    (:work 1:52.7 400 :hr (165 :max 172))
    (:work 1:55.2 400 :hr (173 :max 174))
    (:work 1:53.7 400 :hr (175 :max 175))
    (:rest 0:40.8   0 :hr (171 :min 164))
    (:work 1:53.5 400 :hr (168 :max 173))
    (:work 1:55.5 400 :hr (174 :max 176))
    (:work 1:54.0 400 :hr (175 :max 176))
    (:rest 0:40.9   0 :hr (172 :min 165))
    (:work 1:55.1 400 :hr (168 :max 175))
    (:work 1:52.3 400 :hr (177 :max 178))
    (:work 1:54.0 400 :hr (178 :max 178))
    (:rest 0:41.1   0 :hr (175 :min 169))
    (:work 1:54.4 400 :hr (171 :max 176))
    (:work 1:56.5 400 :hr (177 :max 177))
    (:work 1:52.6 400 :hr (178 :max 178))
    (:rest 0:42.3   0 :hr (175 :min 169))
    (:work 1:54.1 400 :hr (172 :max 178))
    (:work 1:54.6 400 :hr (178 :max 179))
    (:work 1:53.2 400 :hr (179 :max 180))
   ;(:rest 5:29.5   0 :hr (140 :min 127))
    (:cool 4:07.5 800 :hr (160 :max 170))
   ;(:rest 2:36.8   0 :hr (144 :min 130))
    (:cool 2:16.1 400 :hr (147 :max 156))
    (:cool 2:21.2 400 :hr (155 :max 157))
    (:cool 2:26.6 400 :hr (155 :max 158)))

(defworkout "2008-07-29" "20.58"
    (:warm 9:00   1500)
    (:warm 7:48.7 1500)
    (:work 0:45.1  200)
    (:rest 0:12.0    0)
    (:work 0:45.3  200)
    (:rest 0:10.7    0)
    (:work 0:42.0  200)
    (:rest 0:11.2    0)
    (:work 0:44.0  200)
    (:rest 8:13.4  400)
    (:work 0:41.5  200)
    (:rest 0:11.3    0)
    (:work 0:43.0  200)
    (:rest 0:11.0    0)
    (:work 0:41.4  200)
    (:rest 0:11.2    0)
    (:work 0:42.1  200)
    (:rest 8:29.5  400)
    (:work 0:43.3  200)
    (:rest 0:12.2    0)
    (:work 0:42.6  200)
    (:rest 0:10.6    0)
    (:work 0:42.6  200)
    (:rest 0:12.4    0)
    (:work 0:41.5  200)
   ;(:pause 6:28.0)
    (:cool 5:22.5 1000)
   ;(:rest 2:30.5    0)
    (:cool 9:17.5 1500))

(defworkout "2008-07-31" "20.39"
    (:warm 8:55.0 1500)
    (:warm 7:41.7 1500)
    (:work 0:59.9 1912/10)
    (:work 0:47.7 1587/10)
    (:work 0:45.2 1587/10)
    (:work 0:46.2 1587/10)
    (:work 0:45.8 1587/10)
    (:work 0:44.5 1587/10)
    (:work 0:46.3 1587/10)
    (:work 0:45.4 1587/10)
    (:work 0:45.0 1587/10)
    (:work 0:44.4 1587/10)
    (:work 0:44.7 1587/10)
    (:work 0:44.4 1587/10)
    (:work 0:46.8 1587/10)
    (:work 0:46.4 1587/10)
    (:work 0:46.7 1587/10)
    (:work 0:45.5 1587/10)
    (:work 0:46.3 1587/10)
    (:work 0:46.2 1587/10)
    (:work 0:46.0 1587/10)
    (:work 0:44.6 1587/10)
    (:work 0:45.3 1587/10)
    (:work 0:44.8 1587/10)
    (:work 0:45.6 1587/10)
    (:work 1:30.6 3174/10)
    (:cool 7:54.4 1500)
    (:cool 8:47.6 1500))

(defworkout "2008-08-04" "13.04"
    (:warm 9:41.0 1600)
    (:warm 8:39.3 1600)
    (:work 1:39.7  400)
    (:rest 0:39.9    0)
    (:work 1:38.7  400)
    (:rest 0:40.3    0)
    (:work 1:34.9  400)
    (:rest 0:40.4    0)
    (:work 1:33.7  400)
    (:rest 9:00.4  400)
    (:work 1:36.2  400)
    (:rest 0:40.5    0)
    (:work 1:32.0  400)
    (:rest 0:40.1    0)
    (:work 1:34.3  400)
    (:rest 0:40.3    0)
    (:work 1:31.5  400)
    (:rest 9:26.7  400)
    (:work 1:35.4  400)
    (:rest 0:41.2    0)
    (:work 1:33.0  400)
    (:rest 0:41.1    0)
    (:work 1:32.9  400)
    (:rest 0:40.1    0)
    (:work 1:24.9  400)
    (:cool 6:13.1 1200)
    (:cool 4:33.1  800))

(defworkout "2008-08-07" "14.49"
    (:warm 9:01   1500)
    (:warm 8:00   1500)
    (:work 1:42.6  400)
    (:rest 1:00.3    0)
    (:work 1:38.4  400)
    (:rest 1:00.2    0)
    (:work 1:38.7  400)
    (:rest 1:00.5    0)
    (:work 1:38.6  400)
    (:rest 1:00.3    0)
    (:work 1:38.0  400)
    (:rest 1:00.3    0)
    (:work 1:32.3  400)
    (:rest 1:00.4    0)
    (:work 1:31.7  400)
    (:rest 1:00.2    0)
    (:work 1:25.7  400)
    (:cool 7:29.9 1500)
    (:cool 8:37.2 1500))

(defworkout "2008-08-10" "13.44"
    (:warm 8:46.3 1500)
    (:warm 7:32.3 1500)
    ;; 14:15
    (:work 1:32.4  400)
    (:rest 0:30.5    0)
    (:work 1:32.9  400)
    (:rest 0:31.3    0)
    (:work 1:29.2  400)
   ;(:pause 9:59.9)
    (:work 1:32.7  400)
    (:rest 0:30.6    0)
    (:work 1:31.5  400)
    (:rest 0:29.7    0)
    (:work 1:28.3  400)
    (:cool 4:51.4 1000)
    (:cool 5:30.9 1000))

(defworkout "2008-08-13" "19.38"
    (:warm 9:09.6 1600)
    (:warm 1:51.1  400)
    (:warm 0:53.9  200)
    (:work 0:21.5  100)
    (:work 0:47.7  200)
    (:work 0:48.8  200)
    (:work 0:47.4  200)
    (:work 0:48.8  200)
    (:work 0:47.8  200)
    (:work 0:46.8  200)
    (:work 0:41.6  200)
    (:cool 4:30.2  800))

(defworkout "2008-08-15" "13.48"
    (:warm 8:57.6 1500)
    (:warm 7:38.0 1500)
    ;; 14:20
    (:work 0:46.6  200)
    (:rest 0:58.0    0)
    (:work 0:41.9  200)
    (:rest 0:58.7    0)
    (:work 0:40.5  200)
    (:rest 0:57.3    0)
    (:work 0:39.7  200)
    (:rest 0:56.9    0)
    (:work 0:40.3  200)
    ;(:rest 7:58.8  400)
    (:rest 2:20  400)
    (:work 0:43.7  200)
    (:rest 0:57.6    0)
    (:work 0:42.9  200)
    (:rest 0:57.3    0)
    (:work 0:42.5  200)
    (:rest 1:00.2    0)
    (:work 0:40.8  200)
    (:rest 0:59.9    0)
    (:work 0:37.8  200)
    (:cool 4:53.8 1000)
    (:cool 5:29.7 1000))

(defworkout "2008-08-16" "11.52"
    (:warm 5:51.6    1000 :hr (143 :max 149))
   ;(:pause 5:45.6      0 :hr (128 :min 118))
    (:warm 5:12.2    1000 :hr (157 :max 169))
   ;(:pause 8:57.1      0 :hr (122 :min 119))
    ;; 12:18
    (:work 0:53.7 1912/10 :hr (144 :max 160))
    (:work 0:45.7 1587/10 :hr (165 :max 166))
    (:work 0:47.6 1587/10 :hr (166 :max 167))
    (:work 0:45.8 1587/10 :hr (166 :max 167))
    (:work 0:45.5 1587/10 :hr (169 :max 170))
    (:work 0:44.6 1587/10 :hr (171 :max 173))
    (:work 0:45.9 1587/10 :hr (173 :max 173))
    (:work 0:44.9 1587/10 :hr (173 :max 174))
    (:work 0:46.0 1587/10 :hr (174 :max 176))
    (:work 0:45.5 1587/10 :hr (174 :max 175))
    (:work 0:45.4 1587/10 :hr (175 :max 176))
    (:work 0:44.8 1587/10 :hr (176 :max 177))
    (:work 0:45.0 1587/10 :hr (176 :max 178))
    (:work 0:44.8 1587/10 :hr (177 :max 177))
    (:work 0:44.9 1587/10 :hr (177 :max 179))
    (:work 0:44.4 1587/10 :hr (178 :max 179))
    (:work 0:44.8 1587/10 :hr (179 :max 180))
    (:work 0:45.0 1587/10 :hr (179 :max 180))
    (:work 0:46.1 1587/10 :hr (178 :max 179))
    (:work 0:45.6 1587/10 :hr (178 :max 179))
    (:work 0:45.9 1587/10 :hr (180 :max 181))
    (:work 0:43.7 1587/10 :hr (181 :max 182))
    (:work 0:44.2 1587/10 :hr (180 :max 181))
    (:work 0:42.7 1587/10 :hr (181 :max 182))
    (:work 0:41.9 1587/10 :hr (182 :max 183))
   ;(:pause 9:19.1      0 :hr (132 :min 118))
    (:cool 5:16.8 1000    :hr (161 :max 171)))

(defworkout "2008-08-20" "19.38"
    (:warm 8:26.0 1600)
    (:warm 4:08.6  800)
    (:warm 0:18.6  100)
    (:warm 0:15.8   80)
    (:warm 0:04.1   20)
    (:warm 0:03.4   20)
    (:warm 0:13.0   60)
    (:warm 0:04.2   20)
    (:warm 0:04.4   20)
    (:warm 0:13.3   60)
    ;; 20:25
    (:work 0:42.9  200 :hr (162 :max 173))
    (:work 0:44.2  200 :hr (170 :max 177))
    (:work 0:41.7  200 :hr (184 :max 188))
    (:work 0:35.0  200 :hr (188 :max 190))
    (:cool 6:13.2 1200))

(defworkout "2008-08-23" "21.40"
    (:warm 6:47.4 1.2  :hr (150 :max 166))
    (:warm 3:00.4 0.55 :hr (163 :max 170))
    ; 22:07:50
    (:work 10:19.4 2.0 :hr (168 :max 176))
    (:work  4:49.6 1.0 :hr (176 :max 180))
    (:work  5:09.0 1.0 :hr (177 :max 180))
    (:work  4:53.1 1.0 :hr (179 :max 183))
    (:work  4:55.8 1.0 :hr (180 :max 182))
    (:work 10:06.5 2.0 :hr (180 :max 182))
    (:work  4:52.5 1.0 :hr (183 :max 186))
    (:work  4:22.4 1.0 :hr (184 :max 187))
    (:cool  3:00   0.5))

(defworkout "2008-08-24" "14.42"
    (:warm 5:44.2 1000)
    (:warm 4:59.2 1000)
    ;; 15:03
    (:work 0:46.5  200)
    (:work 0:52.0  200)
    (:work 0:51.8  200)
    (:rest 1:58.7    0)
    (:work 1:36.6  400)
    (:work 0:47.6  200)
    (:rest 1:59.5    0)
    (:work 0:45.6  200)
    (:work 0:48.8  200)
    (:work 0:49.3  200)
    (:rest 1:56.7    0)
    (:work 0:48.6  200)
    (:work 0:49.6  200)
    (:work 0:48.9  200)
    (:rest 1:58.3    0)
    (:work 0:47.9  200)
    (:work 0:46.5  200)
    (:work 0:44.5  200)
    (:cool 5:31.3 1000))

(defworkout "2008-08-27" "19.34"
    (:warm 8:58.2 1600 :hr (154 :max 160))
    (:warm 3:56.2  800 :hr (163 :max 176))
    ;; 20:02
    (:work 0:51.8  200 :hr (162 :max 170))
    (:work 1:43.0  400 :hr (175 :max 180)) ;159/180
    (:work 1:40.0  400 :hr (184 :max 186)) ;184/192
    (:work 1:39.0  400 :hr (186 :max 188)) ;170/194
    (:work 1:41.2  400 :hr (188 :max 189))
    (:work 0:51.0  200 :hr (188 :max 189))
    (:work 0:50.2  200 :hr (188 :max 189))
    (:work 1:42.7  400 :hr (189 :max 190))
    (:work 1:35.8  400 :hr (190 :max 191))
    (:cool 6:53.7 1200 :hr (153 :max 161)))

(defworkout "2008-08-31" "15.36"
    (:warm 8:48.4 1500 :hr (142 :max 156))
    (:warm 3:59.7  800 :hr (156 :max 176))
    (:work 1:56.7  400 :hr (153 :max 166))
    (:work 1:59.9  400 :hr (167 :max 170))
    (:work 0:58.0  200 :hr (172 :max 175))
    (:work 2:28.4  500 :hr (173 :max 176))
    (:rest 1:07.8    0 :hr (160 :min 144))
    (:work 2:30.8  500 :hr (161 :max 171))
    (:work 0:59.0  200 :hr (171 :max 174))
    (:work 0:57.3  200 :hr (174 :max 175))
    (:work 0:57.9  200 :hr (175 :max 176))
    (:work 0:58.4  200 :hr (176 :max 178))
    (:work 0:59.4  200 :hr (177 :max 178))
    (:rest 1:05.9    0 :hr (166 :min 148))
    (:work 0:57.0  200 :hr (154 :max 165))
    (:work 0:59.7  200 :hr (170 :max 173))
    (:work 1:58.8  400 :hr (172 :max 179))
    (:work 0:59.0  200 :hr (175 :max 177))
    (:work 2:27.5  500 :hr (171 :max 178)))

(defworkout "2008-08-31" "16.30"
    (:work 0:08.6 60))

(defworkout "2008-09-04" "16.42"
    (:warm 8:53.3 1500 :hr (148 :max 155))
    (:warm 4:58.1 1000 :hr (161 :max 173))
    ;; 17:09
    (:work 0:50.5  200 :hr (155 :max 164))
    (:work 0:51.2  200 :hr (175 :max 178))
    (:work 0:52.6  200 :hr (179 :max 180))
    (:work 0:50.3  200 :hr (179 :max 180))
    (:work 0:52.0  200 :hr (179 :max 181))
    (:rest 2:59.3    0 :hr (141 :min 126))
    (:work 0:46.7  200 :hr (152 :max 169))
    (:work 0:54.4  200 :hr (174 :max 177))
    (:work 0:52.2  200 :hr (178 :max 179))
    (:work 0:50.0  200 :hr (180 :max 182))
    (:work 0:49.9  200 :hr (181 :max 183))
    (:rest 2:58.7    0 :hr (145 :min 128))
    (:work 0:50.3  200 :hr (148 :max 167))
    (:work 0:50.9  200 :hr (174 :max 178))
    (:work 0:52.6  200 :hr (180 :max 181))
    (:work 0:51.6  200 :hr (181 :max 182))
    (:work 0:51.5  200 :hr (182 :max 183))
    (:rest 3:04.7    0 :hr (144 :min 131))
    (:work 0:49.6  200 :hr (149 :max 169))
    (:work 0:50.6  200 :hr (175 :max 180))
    (:work 0:50.7  200 :hr (181 :max 182))
    (:work 0:50.3  200 :hr (182 :max 183))
    (:work 0:47.9  200 :hr (183 :max 183))
    ;; 12:16.5, 128\125
    (:work 0:09.4   60)
    (:rest 6:17.5    0 :hr 123)
    (:work 0:08.5   60)
    (:rest 6:24.3    0 :hr 126)
    (:work 0:08.4   60)
    ;; 5:34.0, 128\120
    (:cool 8:46.9 1500 :hr (152 :max 158)))

(defworkout "2008-09-07" "16.07"
    (:warm 8:47.5 1500 :hr (148 :max 158))
    (:warm 5:08.9 1000 :hr (160 :max 174))
    ;; 16:34
    (:work 0:56.1 1912/10 :hr (144 :max 157))
    (:work 1:36.2 3174/10 :hr (165 :max 168))
    (:work 1:33.9 3174/10 :hr (170 :max 171))
    (:work 0:46.0 1587/10 :hr (172 :max 173))
    (:work 0:47.6 1587/10 :hr (174 :max 175))
    (:work 1:33.1 3174/10 :hr (175 :max 176))
    (:work 1:33.6 3174/10 :hr (175 :max 176))
    (:work 1:34.3 3174/10 :hr (175 :max 177))
    (:work 1:34.3 3174/10 :hr (176 :max 177))
    (:work 1:34.2 3174/10 :hr (175 :max 176))
    (:work 1:32.2 3174/10 :hr (177 :max 178))
    (:work 1:33.1 3174/10 :hr (176 :max 178))
    (:work 1:31.6 3174/10 :hr (177 :max 179))
    (:work 1:31.6 3174/10 :hr (177 :max 179)))

(defworkout "2008-09-07" "16.54"
    ;;(:rest 2:03.1)
    (:warm 2:08.5 400 :hr (154 :max 162))
    (:rest 11:04.9 0)
    (:work 0:09.4  60)
    (:rest 5:06.6   0)
    (:work 0:08.3  60)
    (:rest 6:00.2   0)
    (:work 0:08.4  60)
    ;; 3:31.0
    (:cool 5:48.1 1000))

(defworkout "2008-09-09" "11.33"
    (:work 6:17.8 1.0)
    (:work 6:14.3 1.0)
    (:work 6:17.7 1.0)
    (:work 6:28.0 1.0)
    (:work 6:26.0 1.0)
    (:work 5:56.0 1.0)
    (:work 5:46.2 1.0)
    (:work 0:34.4 0.08))

(defworkout "2008-09-11" "17.48"
    (:warm 5:18.5 0.98  :hr (160 :max 171))
    ;; 17:59:58
    (:work 0:06.0 0.025 :hr (148 :max 148))
    (:work 0:24.8 0.100 :hr (164 :max 173))
    (:work 0:28.6 0.115 :hr (176 :max 178))
    (:work 3:18.5 0.760 :hr (182 :max 188))
    (:work 7:01.6 1.66  :hr (184 :max 190)))

(defworkout "2008-09-15" "11.34"
    (:work 37:16.2 6.4))

(defworkout "2008-09-22" "11.11"
    (:work 6:01.8 1.0)
    (:work 6:00.0 1.0)
    (:work 6:02.0 1.0)
    (:work 6:02.0 1.0)
    (:work 5:28.0 1.0)
    (:work 6:00.1 1.0)
    (:work 5:53.9 1.0)
    (:work 0:31.0 0.08))

(defworkout "2008-09-25" "15.47"
    (:warm 6:09.9 1.0 :hr (140 :max 153))
    (:warm 5:59.9 1.0 :hr (153 :max 159))
    (:warm 5:46.1 1.0 :hr (158 :max 174))
    (:warm 5:42.3 1.0 :hr (158 :max 173))
    (:work 5:09.9 1.0 :hr (168 :max 172))
    (:work 5:04.0 1.0 :hr (172 :max 177))
    (:work 4:54.1 1.0 :hr (173 :max 175))
    (:work 5:15.7 1.0 :hr (170 :max 173))
    (:cool 5:52.0 1.0 :hr (163 :max 171))
    (:cool 0:57.9 0.15 :hr (165 :max 166)))

(defworkout "2008-09-26" "11.30"
    (:work 2:30   0.4)
    (:work 6:23.9 1.0)
    (:work 6:18.0 1.0)
    (:work 6:21.9 1.0)
    (:work 6:12.2 1.0)
    (:work 6:15.9 1.0)
    (:work 6:24.0 1.0)
    (:work 1:30.3 0.22))

(defworkout "2008-09-28" "11.34"
    (:warm 6:23.8 1.0 :hr (136 :max 145))
    (:work 6:20.0 1.0 :hr (146 :max 155))
    (:work 6:12.1 1.0 :hr (148 :max 153))
    (:work 6:18.0 1.0 :hr (149 :max 153))
    (:work 6:18.0 1.0 :hr (151 :max 154))
    (:work 6:20.0 1.0 :hr (148 :max 152))
    (:work 6:15.9 1.0 :hr (146 :max 152))
    (:work 6:20.2 1.0 :hr (151 :max 154))
    (:work 6:29.8 1.0 :hr (150 :max 156))
    (:work 6:26.0 1.0 :hr (149 :max 152))
    (:work 6:18.3 1.0 :hr (150 :max 153))
    (:work 6:21.7 1.0 :hr (152 :max 155))
    (:work 6:22.0 1.0 :hr (153 :max 158))
    (:cool 0:18.2 0.04 :hr (155 :max 155)))

(defworkout "2008-09-29" "11.10"
  (:warm 6:09.9 1.0 :hr (138 :max 147))
  (:work 6:02.0 1.0 :hr (146 :max 151))
  (:work 6:09.9 1.0 :hr (146 :max 149))
  (:work 6:02.3 1.0 :hr (147 :max 151))
  (:work 6:09.7 1.0 :hr (148 :max 152))
  (:work 6:06.0 1.0 :hr (150 :max 152))
  (:cool 6:04.2 0.98 :hr (151 :max 156)))

(defworkout "2008-10-02" "16.39"
  (:warm 8:45.0 1500 :hr (151 :max 162))
  (:warm 3:37.8  800 :hr (170 :max 179))
  ;;(:work 0:08.9   60 :hr (136 :max 168))
  ;;(:work 0:08.4   60 :hr (152 :max 169))
  (:work 1:23.9 3328/10 :hr (154 :max 167))
  (:work 1:21.4 3328/10 :hr (170 :max 175))
  (:work 1:41.9 3328/10 :hr (173 :min 171))
  (:work 1:41.3 3328/10 :hr (170 :min 170))
  (:work 1:26.1 3328/10 :hr (176 :max 178))
  (:work 1:25.7 3328/10 :hr (181 :max 182))
  (:work 3:24.6 3328/5  :hr (173 :min 172))
  (:work 1:28.1 3328/10 :hr (178 :max 180))
  (:work 1:29.6 3328/10 :hr (178 :max 179))
  (:work 3:23.6 3328/5  :hr (173 :min 170))
  (:cool 5:42.5 9984/10 :hr 150))

(defworkout "2008-10-03" "11.58"
  (:work 41:49.7 6.54 :hr (147 :max 154)))

(defworkout "2008-10-05" "19.32"
  (:work 1:00   0.17 :hr (135 :max 140))
  (:work 7:01.6 1.2 :hr (140 :max 147))
  (:work 5:48.0 1.0 :hr (155 :max 164))
  (:work 5:44.0 1.0 :hr (155 :max 163))
  (:work 5:52.1 1.0 :hr (158 :max 162))
  (:work 5:15.7 0.86 :hr (162 :max 165)))

(defworkout "2008-10-09" "17.11"
  (:warm 9:05.1 1500 :hr (146 :max 153))
  (:warm 5:06.5 1000 :hr (162 :max 173))
  (:work 5:01.1 1000 :hr (160 :max 169))
  (:rest 1:01.4    0 :hr (153 :min 137))
  (:work 4:56.8 1000 :hr (166 :max 171))
  (:rest 0:59.1    0 :hr (157 :min 138))
  (:work 4:53.4 1000 :hr (166 :max 175))    
  (:rest 1:00.0    0 :hr (158 :min 139))
  (:work 4:53.0 1000 :hr (167 :max 174))
  (:rest 0:59.9    0 :hr (160 :min 131))
  (:work 4:52.8 1000 :hr (167 :max 173))
  (:cool 8:46.2 1500 :hr (150 :max 155)))

(defworkout "2008-10-10" "11.18"
  (:work 6:15.8 1.0 :hr (134 :max 145))
  (:work 6:04.0 1.0 :hr (146 :max 151))
  (:work 6:14.0 1.0 :hr (148 :max 151))
  (:work 6:16.4 1.0 :hr (148 :max 152))
  (:work 6:09.8 1.0 :hr (150 :max 154))
  (:work 6:08.1 1.0 :hr (150 :max 155))
  (:work 1:39.0 0.25 :hr (149 :max 151)))

(defworkout "2008-10-11" "11.00"
  (:work 6:07.6 1.0 :hr (137 :max 145))
  (:work 5:53.0 1.0 :hr (147 :max 151))
  (:work 5:48.8 1.0 :hr (150 :max 156))
  (:work 5:56.3 1.0 :hr (150 :max 153))
  (:work 5:50.5 1.0 :hr (153 :max 163))
  (:work 6:16.5 1.0 :hr (151 :max 155))
  (:work 6:03.9 1.0 :hr (153 :max 158))
  (:work 6:09.9 1.0 :hr (154 :max 159))
  (:work 5:58.2 1.0 :hr (157 :max 161))
  (:work 6:09.9 1.0 :hr (157 :max 163))
  (:work 6:19.4 1.0 :hr (155 :max 159))
  (:work 12:25.3 2.0 :hr (155 :max 161))
  (:work 6:06.9 1.0 :hr (157 :max 161))
  (:work 6:16.0 1.0 :hr (158 :max 162))
  (:work 6:06.3 1.0 :hr (157 :max 162))
  (:work 6:00.3 1.0 :hr (163 :max 176))
  (:work 6:21.3 1.0 :hr (164 :max 175))
  (:work 5:24.9 1.0 :hr (173 :max 177))
  (:work 4:50.1 1.0 :hr (182 :max 185))
  (:work 6:05.1 1.0 :hr (173 :max 182))
  (:work 0:14.8 100 :hr (182 :max 186)))

(defworkout "2008-10-12" "15.05"
  ;(:warm 29:26.9   1 :hr (140 :max 154))
  (:work  1:50.5 385 :hr (152 :max 164)))
  ;(:cool 35:32.3   1 :hr (138 :max 155)))

(defworkout "2008-10-13" "11.12"
  (:warm 6:27.8 1.0 :hr (138 :max 144))
  (:work 6:12.2 1.0 :hr (148 :max 152))
  (:work 6:09.9 1.0 :hr (149 :max 153))
  (:work 6:16.0 1.0 :hr (150 :max 153))
  (:work 6:23.9 1.0 :hr (150 :max 153))
  (:work 6:14.0 1.0 :hr (153 :max 157))
  (:work 3:17.7 0.52 :hr (152 :max 155)))

(defworkout "2008-10-16" "16.37"
  (:warm 9:00.3 1500   :hr (145 :max 155))
  (:warm 3:53.9  800   :hr (166 :max 174))
  (:warm 0:23.1  100   :hr (126 :max 158))
  (:warm 0:20.8  100   :hr (152 :max 159))
  (:work 0:42.6  500/3 :hr (156 :max 164))
  (:work 0:44.6  500/3 :hr (171 :max 175))
  (:work 0:43.0  500/3 :hr (177 :max 178))
  (:work 0:50.1  500/3 :hr (178 :min 177))
  (:work 0:50.6  500/3 :hr (175 :min 172))
  (:work 0:52.3  500/3 :hr (170 :min 170))
  (:work 0:44.3  500/3 :hr (176 :max 179))
  (:work 0:44.6  500/3 :hr (178 :max 180))
  (:work 0:43.1  500/3 :hr (182 :max 183))
  (:work 0:51.8  500/3 :hr (179 :min 177))
  (:work 0:50.8  500/3 :hr (176 :min 176))
  (:work 0:53.1  500/3 :hr (173 :min 171))
  (:work 0:45.5  500/3 :hr (174 :max 177))
  (:work 0:45.1  500/3 :hr (177 :max 178))
  (:work 0:44.1  500/3 :hr (180 :max 181))
  (:work 0:54.7  500/3 :hr (178 :min 174))
  (:work 0:51.6  500/3 :hr (174 :min 172))
  (:work 0:52.5  500/3 :hr (171 :min 171))
  (:work 0:45.6  500/3 :hr (174 :max 176))
  (:work 0:44.9  500/3 :hr (177 :max 179))
  (:work 0:44.3  500/3 :hr (180 :max 181))
  (:work 1:42.0 1000/3 :hr (176 :min 173))
  (:work 0:52.8  500/3 :hr (171 :min 171))
  (:cool 5:49.9 1000   :hr (151 :max 156)))

(defworkout "2008-10-17" "11.21"
  (:work 6:17.8 1.0)
  (:work 6:06.0 1.0)
  (:work 6:14.0 1.0)
  (:work 6:12.1 1.0)
  (:work 6:05.9 1.0)
  (:work 6:06.0 1.0)
  (:work 6:02.0 1.0)
  (:work 2:26.3 0.39))

(defworkout "2008-10-19" "11.03"
  (:warm 6:29.8 1.0 :hr (139 :max 148))
  (:work 6:24.0 1.0 :hr (148 :max 155))
  (:work 6:24.3 1.0 :hr (150 :max 155))
  (:work 6:33.7 1.0 :hr (150 :max 154))
  (:work 7:46.1 1.0 :hr (157 :max 164))
  (:work 5:56.2 1.0 :hr (153 :max 164))
  (:work 6:23.8 1.0 :hr (153 :max 158))
  (:work 6:13.9 1.0 :hr (152 :max 156))
  (:work 6:22.2 1.0 :hr (151 :max 158))
  (:work 6:13.9 1.0 :hr (150 :max 155))
  (:work 6:17.9 1.0 :hr (152 :max 157))
  (:work 6:22.0 1.0 :hr (151 :max 154))
  (:work 6:22.3 1.0 :hr (153 :max 158))
  (:cool 5:17.1 0.81 :hr (150 :max 153)))

(defworkout "2008-10-20" "10.57"
  (:warm 6:23.9 1.0 :hr (135 :max 144))
  (:work 6:10.0 1.0 :hr (145 :max 152))
  (:work 6:00.1 1.0 :hr (150 :max 154))
  (:work 6:01.9 1.0 :hr (149 :max 154))
  (:work 6:11.9 1.0 :hr (151 :max 154))
  (:work 6:12.3 1.0 :hr (150 :max 153))
  (:work 6:05.8 1.0 :hr (150 :max 153))
  (:work 2:35.0 0.4 :hr (153 :max 156)))

(defworkout "2008-10-23" "16.10"
  (:warm 6:08.0 1.0  :hr (140 :max 152))
  (:warm 5:55.9 1.0  :hr (153 :max 159))
  (:warm 5:33.9 1.0  :hr (163 :max 171))
  (:work 5:00.0 1.0  :hr (172 :max 174))
  (:work 2:35.5 0.51 :hr (174 :max 176))
  (:work 2:18.5 0.49 :hr (175 :max 177))
  (:work 4:52.2 1.0  :hr (176 :max 178))
  (:work 4:42.0 1.0  :hr (177 :max 179))
  (:cool 5:18.0 1.0  :hr (172 :max 179 :min 168))
  (:cool 5:53.8 1.0  :hr (162 :max 171))
  (:cool 0:45.4 0.1  :hr (158 :max 160)))

(defworkout "2008-10-24" "11.44"
  (:warm 6:15.8 1.0)
  (:work 6:04.0 1.0)
  (:work 6:02.0 1.0)
  (:work 5:52.0 1.0)
  (:work 6:12.0 1.0)
  (:work 6:12.0 1.0)
  (:work 6:02.0 1.0)
  (:work 3:08.1 0.51))

(defworkout "2008-10-26" "10.21"
  (:warm 6:23.9 1.00 :hr (141 :max 151))
  (:warm 6:22.2 1.00 :hr (147 :max 152))
  (:work 6:15.8 1.00 :hr (149 :max 152)) ;Platt
  (:work 2:35.1 0.39 :hr (152 :max 154)) ;Majnabbe
  (:work 3:53.0 0.61 :hr (154 :max 159)) ;Svagt uppfor
  (:work 1:21.0 0.20 :hr (158 :max 160)) ;Svagt uppfor
  (:work 6:44.8 0.80 :hr (151 :max 157)) ;Uppfor Alvsborgsbron
  (:work 5:40.2 1.00 :hr (145 :max 150)) ;Nerfor Alvsborgsbron
  (:work 2:09.1 0.36 :hr (151 :max 153)) ;Nerfor Alvsborgsbron
  (:work 4:02.8 0.64 :hr (151 :max 155)) ;Platt
  (:work 6:16.2 1.00 :hr (152 :max 155)) ;Platt
  (:work 6:23.8 1.00 :hr (151 :max 156)) ;Platt
  (:work 6:24.0 1.00 :hr (151 :max 154)) ;Platt
  (:work 6:27.9 1.00 :hr (149 :max 153)) ;Platt
  (:work 6:22.3 1.00 :hr (150 :max 154)) ;Platt
  (:work 5:21.4 0.85 :hr (152 :max 155)) ;Platt
  (:work 0:42.1 0.12 :hr (151 :max 152)) ;Svagt nerfor
  (:work 0:08.3 0.03 :hr (153 :max 153)) ;Svagt nerfor
  (:work 6:03.2 0.72 :hr (148 :max 155)) ;Uppfor Gota Alvbron
  (:work 1:30.7 0.28 :hr (147 :max 148)) ;Nerfor Gota Alvbron
  (:work 1:42.2 0.28 :hr (148 :max 149)) ;Nerfor Gota Alvbron
  (:work 3:05.5 0.49 :hr (148 :max 152)) ;Platt
  (:cool 1:44.4 0.23 :hr (146 :max 149))
  (:cool 0:24.2 0.05 :hr (144 :max 144)))

(defworkout "2008-10-27" "11.13"
  (:warm 6:27.9 1.0 :hr (134 :max 140))
  (:work 6:14.0 1.0 :hr (144 :max 148))
  (:work 5:59.9 1.0 :hr (148 :max 152))
  (:work 6:00.0 1.0 :hr (149 :max 153))
  (:work 6:08.0 1.0 :hr (149 :max 151))
  (:work 6:10.0 1.0 :hr (149 :max 153))
  (:work 6:12.3 1.0 :hr (151 :max 154))
  (:work 2:04.9 0.33 :hr (149 :max 151)))

(defworkout "2008-10-30" ""
  (:warm 38:00 5.0))

(defworkout "2008-11-03" "11.58"
  (:warm 6:23.9 1.0 :hr (127 :max 138))
  (:warm 6:20.2 1.0 :hr (136 :max 144))
  (:work 6:13.7 1.0 :hr (140 :max 145))
  (:work 6:16.1 1.0 :hr (141 :max 146))
  (:work 6:12.0 1.0 :hr (142 :max 147))
  (:work 6:12.0 1.0 :hr (143 :max 149))
  (:work 6:06.0 1.0 :hr (147 :max 152))
  (:work 1:34.7 0.25 :hr (144 :max 145)))

(defworkout "2008-11-07" "11.17"
  (:warm 6:35.8 1.0 :hr (131 :max 139))
  (:warm 6:24.0 1.0 :hr (138 :max 144))
  (:work 6:10.0 1.0 :hr (142 :max 147))
  (:work 6:16.3 1.0 :hr (143 :max 146))
  (:work 6:23.8 1.0 :hr (143 :max 148))
  (:work 6:18.0 1.0 :hr (144 :max 147))
  (:work 6:18.0 1.0 :hr (146 :max 150))
  (:work 6:23.9 1.0 :hr (144 :max 147))
  (:work 0:19.2 0.04 :hr (141 :max 142)))

(defworkout "2008-11-09" "10.31"
  (:warm 7:11.7 1.0 :hr (121 :max 130))
  (:warm 6:36.0 1.0 :hr (132 :max 136))
  (:work 6:16.0 1.0 :hr (140 :max 144))
  (:warm 6:36.1 1.0 :hr (140 :max 145))
  (:work 6:18.0 1.0 :hr (143 :max 146))
  (:work 6:24.2 1.0 :hr (143 :max 145))
  (:work 6:27.9 1.0 :hr (143 :max 147))
  (:work 6:33.9 1.0 :hr (137 :max 145))
  (:work 6:14.0 1.0 :hr (142 :max 146))
  (:work 6:22.0 1.0 :hr (144 :max 148))
  (:cool 6:52.2 1.0 :hr (137 :max 142))
  (:cool 0:34.0 0.07 :hr (136 :max 137)))

(defworkout "2008-11-10" "11.19"
  (:warm 6:39.8 1.0 :hr (123 :max 134))
  (:warm 6:36.0 1.0 :hr (136 :max 141))
  (:work 6:24.1 1.0 :hr (142 :max 145))
  (:work 6:02.1 1.0 :hr (143 :max 147))
  (:work 6:19.8 1.0 :hr (143 :max 147))
  (:work 6:30.3 1.0 :hr (146 :max 151))
  (:work 6:06.1 1.0 :hr (148 :max 154))
  (:cool 2:00.8 0.31 :hr (146 :max 149)))

#|
(defworkout "2008-11-12" "15.59"
  ;; Bike
  (:warm  3 1 :hr (110 :max 114))
  (:warm  3 1 :hr (120 :max 124))
  (:warm  3 1 :hr (131 :max 135))
  (:work 13 1 :hr (140 :max 146))
  ;; Elliptical
  (:work 17 1 :hr (141 :max 147)))
|#

(defworkout "2008-11-14" "11.15"
  (:warm 6:00.6 0.91 :hr (130 :max 140))
  (:warm 6:00.4 0.96 :hr (140 :max 145))
  (:work 5:00.8 0.84 :hr (146 :max 150))
  (:work 5:00.6 0.80 :hr (144 :max 149))
  (:work 5:00.8 0.80 :hr (145 :max 148))
  (:work 5:00.8 0.82 :hr (147 :max 150))
  (:work 6:00.9 0.97 :hr (146 :max 150))
  (:work 7:00.4 1.12 :hr (145 :max 149))
  (:work 5:30.2 0.88 :hr (145 :max 148)))

(defworkout "2008-11-16" "10.50"
  (:work 6:41.9 1.0  :hr (133 :max 136))
  (:work 6:48.0 1.0  :hr (140 :max 145))
  (:work 6:52.2 1.0  :hr (141 :max 144))
  (:work 6:13.8 0.91 :hr (142 :max 145))
  (:rest 0:55.9 0.09 :hr (140 :max 146))
  (:rest 7:12.8 0.80 :hr (138 :max 143))
  (:work 1:11.3 0.20 :hr (143 :max 145))
  (:work 5:56.0 1.0  :hr (139 :max 150))
  (:work 6:01.9 1.0  :hr (142 :max 148))
  (:work 6:28.0 1.0  :hr (146 :max 151))
  (:work 6:18.2 1.0  :hr (145 :max 149))
  (:work 6:24.1 1.0  :hr (145 :max 148))
  (:work 5:32.9 0.87 :hr (143 :max 150))
  (:rest 1:05.1 0.13 :hr (142 :max 146))
  (:rest 3:28.9 0.44 :hr (140 :max 147))
  (:work 3:06.9 0.56 :hr (143 :max 148))
  (:work 6:22.0 1.0  :hr (146 :max 151))
  (:work 5:10.8 0.79 :hr (148 :max 151)))

(defworkout "2008-11-17" "12.05"
  (:warm 6:39.8 1.0 :hr (128 :max 137))
  (:warm 6:30.2 1.0 :hr (141 :max 145))
  (:work 5:55.9 1.0 :hr (151 :max 155))
  (:work 5:53.9 1.0 :hr (156 :max 159))
  (:work 5:46.3 1.0 :hr (161 :max 166))
  (:work 5:39.7 1.0 :hr (164 :max 167))
  (:work 5:00.1 1.0 :hr (172 :max 184))
  (:cool 4:44.4 0.79 :hr (165 :max 180)))

(defworkout "2008-11-21" "11.17"
  (:warm 6:29.8 1.0 :hr (132 :max 140))
  (:warm 6:20.0 1.0 :hr (140 :max 146))
  (:work 5:58.3 1.0 :hr (144 :max 149))
  (:work 6:03.7 1.0 :hr (145 :max 149))
  (:work 6:12.1 1.0 :hr (146 :max 148))
  (:work 6:09.9 1.0 :hr (145 :max 150))
  (:work 6:16.0 1.0 :hr (146 :max 150))
  (:work 6:14.0 1.0 :hr (145 :max 148))
  (:cool 0:20.8 0.05 :hr (148 :max 148)))

#|
(defworkout "2008-11-22" "11.31"
  (:warm  5:01.5 1 :hr (111 :max 119))
  (:warm  4:49.8 1 :hr (121 :max 130))
  (:warm  5:00.8 1 :hr (132 :max 140))
  (:work 17:00.2 1 :hr (140 :max 144))
  (:work  8:00.1 1 :hr (141 :max 148))
  (:work  4:58.1 1 :hr (141 :max 145))
  (:work 14:59.7 1 :hr (140 :max 146))
  (:work 15:01.4 1 :hr (140 :max 145))
  (:cool  4:59.5 1 :hr (124 :max 139))
  (:cool  4:59.8 1 :hr (114 :max 123)))
|#

(defworkout "2008-11-23" "11.35"
  (:warm 6:31.8 1.0 :hr (132 :max 140))
  (:warm 6:26.1 1.0 :hr (142 :max 146))
  (:warm 6:15.8 1.0 :hr (146 :max 152))
  (:warm 6:24.0 1.0 :hr (149 :max 156))
  (:warm 6:04.0 1.0 :hr (154 :max 158))
  (:warm 5:58.2 1.0 :hr (161 :max 168))
  (:work 5:35.9 1.0 :hr (165 :max 168))
  (:work 5:32.3 1.0 :hr (168 :max 175))
  (:work 5:27.8 1.0 :hr (169 :max 177))
  (:work 5:18.1 1.0 :hr (172 :max 176))
  (:cool 5:59.9 1.0 :hr (163 :max 173))
  (:cool 6:30.0 1.0 :hr (156 :max 163))
  (:cool 0:12.0 0.03 :hr (156 :max 157)))

(defworkout "2008-11-24" "10.40"
  (:warm 6:37.8 1.0 :hr (132 :max 140))
  (:warm 6:22.1 1.0 :hr (144 :max 147))
  (:work 6:14.2 1.0 :hr (147 :max 151))
  (:work 6:15.7 1.0 :hr (149 :max 155))
  (:work 6:22.1 1.0 :hr (154 :max 157))
  (:work 6:14.2 1.0 :hr (156 :max 159))
  (:work 6:11.8 1.0 :hr (157 :max 163))
  (:cool 2:17.3 0.35 :hr (154 :max 155)))

(defworkout "2008-11-28" "11.16"
  (:warm 6:37.8 1.0 :hr (128 :max 135))
  (:warm 6:40.2 1.0 :hr (137 :max 145))
  (:work 6:28.1 1.0 :hr (142 :max 148))
  (:work 6:23.8 1.0 :hr (145 :max 151))
  (:work 6:14.2 1.0 :hr (146 :max 150))
  (:work 6:16.1 1.0 :hr (146 :max 157))
  (:work 6:13.7 1.0 :hr (149 :max 153))
  (:work 6:14.1 1.0 :hr (150 :max 155))
  (:work 6:13.9 1.0 :hr (151 :max 154))
  (:cool 1:04.8 0.17 :hr (151 :max 153)))

(defworkout "2008-12-01" "11.14"
  (:warm 6:41.8 1.0 :hr (129 :max 139))
  (:warm 6:24.1 1.0 :hr (142 :max 146))
  (:work 6:12.2 1.0 :hr (147 :max 152))
  (:work 6:05.8 1.0 :hr (151 :max 157))
  (:work 6:12.3 1.0 :hr (147 :max 158))
  (:work 6:18.0 1.0 :hr (148 :max 151))
  (:work 6:15.9 1.0 :hr (152 :max 156))
  (:work 6:13.8 1.0 :hr (153 :max 156))
  (:cool 0:46.6 0.12 :hr (150 :max 151)))

(defworkout "2008-12-04" "11.07"
  (:warm 6:45.9 1.0 :hr (130 :max 136))
  (:warm 6:30.0 1.0 :hr (140 :max 145))
  (:work 6:13.9 1.0 :hr (145 :max 149))
  (:work 6:22.0 1.0 :hr (149 :max 154))
  (:work 6:18.1 1.0 :hr (152 :max 155))
  (:work 2:30.2 0.41 :hr (153 :max 165))
  (:hill 0:54.4 0.19 :hr (170 :max 174))
  (:work 2:25.3 0.40 :hr (162 :max 174))
  (:work 6:08.3 1.0 :hr (154 :max 158))
  (:cool 6:17.8 1.0 :hr (150 :max 156)))

(defworkout "2008-12-07" "11.18"
  (:warm 6:47.9 1.0 :hr (126 :max 135))
  (:warm 6:29.9 1.0 :hr (137 :max 143))
  (:work 6:12.1 1.0 :hr (143 :max 148))
  (:work 6:28.0 1.0 :hr (143 :max 149))
  (:work 6:18.1 1.0 :hr (148 :max 151))
  (:work 6:19.8 1.0 :hr (146 :max 149))
  (:work 6:12.2 1.0 :hr (148 :max 153))
  (:work 6:19.8 1.0 :hr (149 :max 152))
  (:work 6:20.3 1.0 :hr (147 :max 151))
  (:work 6:07.7 1.0 :hr (149 :max 152))
  (:work 6:12.1 1.0 :hr (148 :max 152))
  (:work 6:08.1 1.0 :hr (150 :max 152))
  (:cool 5:37.7 0.85 :hr (147 :max 154)))

(defworkout "2008-12-08" "10.47"
  (:warm 6:39.8 1.00 :hr (130 :max 140))
  (:warm 6:22.0 1.00 :hr (142 :max 148))
  (:warm 6:30.1 1.00 :hr (146 :max 153))
  (:work 5:57.9 1.00 :hr (160 :max 170))
  (:work 3:39.7 0.59 :hr (156 :max 164))
  (:hill 0:54.5 0.21 :hr (171 :max 175))
  (:work 1:11.8 0.20 :hr (169 :max 175))
  (:work 2:41.7 0.42 :hr (156 :max 166))
  (:hill 0:48.2 0.20 :hr (174 :max 179))
  (:work 2:14.3 0.38 :hr (165 :max 179))
  (:work 1:47.9 0.28 :hr (160 :max 165))
  (:hill 0:42.4 0.19 :hr (176 :max 182))
  (:work 3:05.6 0.53 :hr (167 :max 183))
  (:work 6:14.0 1.00 :hr (155 :max 159))
  (:cool 3:42.8 0.56 :hr (150 :max 154)))

(defworkout "2008-12-11" "10.56"
  (:warm 4:18.7 0.64 :hr (124 :max 130))
 ;(:rest 3:39.7 0.00 :hr (107 :max 130))
  (:warm 1:09.5 0.18 :hr (120 :max 132))
  (:warm 6:27.9 1.00 :hr (134 :max 138))
  (:work 6:20.0 1.00 :hr (142 :max 145))
  (:work 6:02.1 1.00 :hr (146 :max 150))
  (:work 5:47.8 0.92 :hr (146 :max 152))
  (:work 0:28.2 0.08 :hr (148 :max 152))
  (:work 4:21.0 0.76 :hr (160 :max 168))
  (:hill 0:55.6 0.20 :hr (169 :max 174))
  (:work 0:13.3 0.04 :hr (173 :max 174))
  (:work 3:40.3 0.58 :hr (155 :max 173))
  (:hill 0:57.1 0.20 :hr (169 :max 173))
  (:work 1:14.6 0.22 :hr (168 :max 174))
  (:work 6:10.3 1.00 :hr (151 :max 159))
  (:cool 3:31.5 0.56 :hr (149 :max 154)))

(defworkout "2008-12-15" "11.33"
  (:warm 7:01.8 1.00 :hr (121 :max 128))
  (:warm 6:32.1 1.00 :hr (130 :max 134))
  (:work 6:01.9 1.00 :hr (140 :max 148))
  (:work 6:10.0 1.00 :hr (147 :max 154))
  (:work 4:29.5 0.73 :hr (150 :max 161))
  (:hill 0:56.6 0.20 :hr (170 :max 175))
  (:work 0:19.9 0.07 :hr (174 :max 175))
  (:work 2:11.8 0.34 :hr (153 :max 172))
  (:hill 0:51.3 0.19 :hr (166 :max 176))
  (:work 2:55.1 0.46 :hr (157 :max 177))
  (:work 0:10.3 0.03 :hr (150 :max 150))
  (:hill 0:53.7 0.19 :hr (166 :max 177))
  (:work 4:45.9 0.79 :hr (154 :max 178))
  (:work 6:05.9 1.00 :hr (151 :max 156))
  (:cool 6:24.0 1.00 :hr (146 :max 150))
  (:cool 2:28.3 0.36 :hr (144 :max 148)))

(defworkout "2008-12-17" "9.41"
  (:warm 6:55.9 1.0 :hr (129 :max 139))
  (:warm 6:24.0 1.0 :hr (139 :max 145))
  (:work 6:08.0 1.0 :hr (145 :max 148))
  (:work 6:19.9 1.0 :hr (147 :max 151))
  (:work 5:58.2 1.0 :hr (152 :max 158))
  (:work 5:35.9 1.0 :hr (157 :max 161))
  (:work 5:02.0 1.0 :hr (166 :max 172))
  (:work 5:23.9 1.0 :hr (164 :max 172))
  (:work 6:16.0 1.0 :hr (151 :max 161))
  (:cool 1:39.3 0.25 :hr (146 :max 148)))

(defworkout "2008-12-26" "15.41"
  (:warm 6:45.8 1.00 :hr (123 :max 136))
  (:warm 6:24.0 1.00 :hr (137 :max 140))
  (:work 6:12.3 1.00 :hr (143 :max 147))
  (:work 6:09.7 1.00 :hr (153 :max 160))
  (:work 1:10.3 0.20 :hr (151 :max 159))
  (:hill 0:53.7 0.17 :hr (165 :max 174))
  (:work 2:10.6 0.35 :hr (156 :max 175))
  (:hill 0:48.4 0.16 :hr (164 :max 174))
  (:work 0:41.0 0.12 :hr (171 :max 176))
  (:work 2:25.4 0.37 :hr (150 :max 160))
  (:hill 0:49.7 0.17 :hr (167 :max 175))
  (:work 2:45.0 0.46 :hr (156 :max 176))
  (:work 6:02.0 1.00 :hr (152 :max 155))
  (:cool 6:24.1 1.00 :hr (148 :max 151))
  (:cool 3:54.8 0.59 :hr (146 :max 148)))

(defworkout "2009-01-04" "11.32"
  (:warm  5:02.3 0.76 :hr (137 :max 146))
  (:warm  4:59.6 0.77 :hr (148 :max 152))
  (:work 15:00.3 2.40 :hr (158 :max 162))
  (:cool  4:58.4 0.71 :hr (154 :max 162)))

(defworkout "2009-01-05" "10.49"
  (:warm 6:49.8 1.00 :hr (127 :max 141))
  (:warm 6:24.0 1.00 :hr (144 :max 148))
  (:work 6:06.1 1.00 :hr (155 :max 158))
  (:work 1:35.3 0.25 :hr (155 :max 156))
  (:work 1:06.0 0.20 :hr (163 :max 167))
  (:work 3:18.9 0.55 :hr (161 :max 169))
  (:work 6:09.8 1.00 :hr (155 :max 159))
  (:cool 6:30.0 1.00 :hr (154 :max 158))
  (:cool 0:24.6 0.06 :hr (152 :max 152)))

(defworkout "2009-01-07" "10.41"
  (:warm 6:33.8 1.00 :hr (132 :max 144))
  (:warm 6:24.0 1.00 :hr (144 :max 148))
  (:work 6:06.3 1.00 :hr (153 :max 159))
  (:work 2:24.7 0.45 :hr (156 :max 163))
  (:hill 0:56.6 0.19 :hr (170 :max 176))
  (:work 2:10.4 0.36 :hr (164 :min 154))
  (:work 2:34.9 0.40 :hr (156 :max 164))
  (:hill 0:56.6 0.19 :hr (173 :max 178))
  (:work 2:24.6 0.41 :hr (167 :min 159))
  (:work 6:05.9 1.00 :hr (155 :max 160))
  (:cool 6:30.1 1.00 :hr (150 :max 156))
  (:cool 1:56.0 0.28 :hr (149 :max 152)))

(defworkout "2009-01-09" "11.05"
  (:warm 6:45.8 1.00 :hr (135 :max 141))
  (:warm 6:24.0 1.00 :hr (147 :max 151))
  (:work 6:16.0 1.00 :hr (152 :max 157))
  (:work 6:14.0 1.00 :hr (155 :max 158))
  (:work 6:08.0 1.00 :hr (158 :max 167))
  (:work 6:10.2 1.00 :hr (156 :max 159))
  (:work 6:07.9 1.00 :hr (157 :max 160))
  (:work 6:13.9 1.00 :hr (155 :max 159))
  (:cool 4:19.7 0.63 :hr (151 :max 157)))

(defworkout "2009-01-11" "12.12"
  (:warm 6:37.9 1.00 :hr (134 :max 144))
  (:warm 6:31.9 1.00 :hr (143 :max 148))
  (:work 6:20.0 1.00 :hr (147 :max 151))
  (:work 6:28.0 1.00 :hr (150 :max 157))
  (:work 6:20.2 1.00 :hr (152 :max 156))
  (:work 6:23.9 1.00 :hr (151 :max 154))
  (:work 6:12.1 1.00 :hr (151 :max 157))
  (:work 6:13.8 1.00 :hr (154 :max 158))
  (:work 5:58.3 1.00 :hr (157 :max 162))
  (:work 4:53.9 1.00 :hr (172 :max 178))
  (:work 5:55.9 1.00 :hr (162 :max 176))
  (:cool 6:03.0 0.90 :hr (154 :max 157)))

(defworkout "2009-01-12" "11.16"
  (:warm 6:49.8 1.00 :hr (132 :max 143))
  (:warm 6:36.1 1.00 :hr (145 :max 155))
  (:work 6:22.1 1.00 :hr (150 :max 156))
  (:work 6:23.8 1.00 :hr (148 :max 151))
  (:work 6:22.3 1.00 :hr (155 :max 161))
  (:work 6:15.9 1.00 :hr (156 :max 161))
  (:work 0:51.3 0.14 :hr (156 :max 159)))

(defworkout "2009-01-15" "15.49"
  (:warm 6:23.8 1.00 :hr (136 :max 142))
  (:warm 6:02.2 1.00 :hr (147 :max 151))
  (:warm 5:26.1 1.00 :hr (160 :max 171))
  (:work 4:53.7 1.00 :hr (174 :max 176))
  (:work 4:54.0 1.00 :hr (175 :max 178))
  (:work 4:48.0 1.00 :hr (178 :max 179))
  (:cool 5:40.0 1.00 :hr (167 :max 178 :min 162))
  (:cool 6:06.1 1.00 :hr (157 :max 162 :min 154))
  (:cool 1:13.0 0.18 :hr (154 :max 155)))

(defworkout "2009-01-16" "11.05"
  (:warm 6:29.8 1.00 :hr (131 :max 139))
  (:warm 6:18.0 1.00 :hr (140 :max 143))
  (:work 6:12.3 1.00 :hr (146 :max 149))
  (:work 6:05.7 1.00 :hr (147 :max 150))
  (:work 6:10.3 1.00 :hr (149 :max 153))
  (:work 6:15.7 1.00 :hr (149 :max 153))
  (:cool 2:31.3 0.39 :hr (148 :max 150)))

(defworkout "2009-01-18" "12.18"
  (:warm 6:48.1 1.00 :hr (131 :max 140))
  (:warm 6:31.8 1.00 :hr (142 :max 145))
  (:work 6:18.0 1.00 :hr (147 :max 151))
  (:work 6:10.1 1.00 :hr (151 :max 154))
  (:work 6:17.8 1.00 :hr (152 :max 159))
  (:work 6:16.3 1.00 :hr (155 :max 160))
  (:work 6:17.7 1.00 :hr (155 :max 160))
  (:work 6:08.2 1.00 :hr (156 :max 160))
  (:work 6:09.8 1.00 :hr (156 :max 159))
  (:work 6:16.0 1.00 :hr (156 :max 162))
  (:work 6:08.1 1.00 :hr (157 :max 161))
  (:work 6:14.2 1.00 :hr (158 :max 160))
  (:work 4:07.7 0.67 :hr (156 :max 159)))

(defworkout "2009-01-19" "11.15"
  (:warm 6:25.9 1.00 :hr (137 :max 146))
  (:warm 6:20.0 1.00 :hr (145 :max 152))
  (:work 6:10.1 1.00 :hr (151 :max 154))
  (:work 5:47.8 1.00 :hr (161 :max 165))
  (:work 0:31.5 0.08 :hr (161 :max 165))
  (:hill 0:54.8 0.22 :hr (172 :max 177))
  (:work 4:15.7 0.70 :hr (163 :max 177))
  (:work 0:24.5 0.07 :hr (163 :max 165))
  (:hill 0:50.8 0.20 :hr (174 :max 178))
  (:work 4:16.8 0.73 :hr (161 :max 178 :min 154))
  (:work 6:08.0 1.00 :hr (153 :max 156 :min 151))
  (:cool 0:27.0 0.07 :hr (153 :max 153)))

(defworkout "2009-01-22" "15.48"
  (:warm 6:01.2 1000 :hr (149 :max 157)) ;1.00km
  (:warm 3:01.1  500 :hr (154 :max 157)) ;0.51km = 1.51km
  ;;(:pause 4:36 0) ;146/159\129
  (:warm 8:12.4 1500 :hr (163 :max 171)) ;1.49km = 1.49km
  ;;(:pause 4:00 0) ;142/170\132
  (:work 0:59.0  200 :hr (150 :max 163)) ;0.19km
  (:work 1:58.7  400 :hr (169 :max 173)) ;0.40km
  (:work 0:28.1  100 :hr (174 :max 175)) ;0.10km
  (:work 0:28.5  100 :hr (175 :max 175)) ;0.11km
  (:work 0:59.3  200 :hr (175 :max 175)) ;0.20km = 1.00km
  (:rest 1:00.4    0 :hr (165 :max 176 :min 148))
  (:work 0:58.8  200 :hr (156 :max 163)) ;0.18km
  (:work 0:58.9  200 :hr (169 :max 171)) ;0.20km
  (:work 0:59.3  200 :hr (174 :max 177)) ;0.20km
  (:work 0:56.8  200 :hr (177 :max 179)) ;0.19km
  (:work 0:59.3  200 :hr (177 :max 179)) ;0.21km = 0.98km
  (:rest 1:00.6    0 :hr (168 :max 176 :min 153))
  (:work 0:58.0  200 :hr (157 :max 165)) ;0.18km
  (:work 1:28.5  300 :hr (172 :max 176)) ;0.31km
  (:work 0:28.5  100 :hr (178 :max 178)) ;0.10km
  (:work 0:57.7  200 :hr (177 :max 179)) ;0.20km
  (:work 1:00.1  200 :hr (176 :max 177)) ;0.21km = 1.00km
  (:rest 1:00.0    0 :hr (164 :max 177 :min 150))
  (:work 0:58.5  200 :hr (154 :max 164)) ;0.16km
  (:work 0:59.6  200 :hr (168 :max 173)) ;0.21km
  (:work 0:59.6  200 :hr (174 :max 175)) ;0.20km
  (:work 0:57.8  200 :hr (175 :max 178)) ;0.20km
  (:work 0:59.6  200 :hr (176 :max 176)) ;0.19km = 0.96km
  ;;(:pause 2:00 0) ;150/176\130
  (:cool 2:43.8  500 :hr (154 :max 162)) ;0.49km
  (:cool 2:45.1  500 :hr (165 :max 166)) ;0.50km = 0.99km
  ;;(:pause 2:45 0) ;141/165\125
  (:cool 2:56.8  500 :hr (150 :max 159)) ;0.48km
  (:cool 3:09.2  500 :hr (154 :max 157)));0.49km = 0.97km = 8.90km

(defworkout "2009-01-23" "10.06"
  (:warm 6:36.0 1.0 :hr (136 :max 145))
  (:warm 6:29.9 1.0 :hr (142 :max 146))
  (:work 6:26.0 1.0 :hr (146 :max 154))
  (:work 6:27.9 1.0 :hr (149 :max 152))
  (:work 6:30.2 1.0 :hr (149 :max 154))
  (:work 6:30.0 1.0 :hr (153 :max 156))
  (:work 6:33.8 1.0 :hr (151 :max 155))
  (:work 1:44.3 .27 :hr (151 :max 155)))

(defworkout "2009-01-25" "10.45"
  (:warm 6:37.7 1.0 :hr (134 :max 145))
  (:warm 6:18.0 1.0 :hr (149 :max 153))
  (:work 6:11.9 1.0 :hr (154 :max 157))
  (:work 6:07.9 1.0 :hr (158 :max 164))
  (:work 6:02.0 1.0 :hr (160 :max 165))
  (:work 5:54.1 1.0 :hr (163 :max 166))
  (:work 5:42.1 1.0 :hr (166 :max 170))
  (:work 5:25.8 1.0 :hr (171 :max 175))
  (:work 5:22.1 1.0 :hr (173 :max 177))
  (:work 5:19.9 1.0 :hr (173 :max 174))
  (:work 5:58.2 1.0 :hr (167 :max 174))
  (:cool 6:23.9 1.0 :hr (160 :max 163))
  (:cool 1:01.6 .15 :hr (159 :max 160)))

(defworkout "2009-01-26" "10.57"
  (:warm 6:53.8 1.0 :hr (131 :max 139))
  (:warm 6:36.0 1.0 :hr (144 :max 153))
  (:work 6:17.9 1.0 :hr (148 :max 153))
  (:work 6:16.2 1.0 :hr (153 :max 162))
  (:work 6:23.9 1.0 :hr (153 :max 155))
  (:work 6:07.9 1.0 :hr (157 :max 162))
  (:cool 6:34.0 1.0 :hr (153 :max 158))
  (:cool 1:07.8 .17 :hr (149 :max 151)))

(defworkout "2009-01-29" "16.23"
  (:warm 6:43.9 1.0 :hr (133 :max 141))
  (:warm 6:06.2 1.0 :hr (146 :max 150))
  (:warm 5:45.9 1.0 :hr (158 :max 165))
  (:work 5:14.0 1.0 :hr (163 :max 166))
  (:work 5:14.0 1.0 :hr (165 :max 167))
  (:work 5:11.9 1.0 :hr (168 :max 169))
  (:work 5:09.9 1.0 :hr (168 :max 171))
  (:work 5:10.2 1.0 :hr (170 :max 173))
  (:cool 5:51.9 1.0 :hr (167 :max 173 :min 162))
  (:cool 6:33.9 1.0 :hr (156 :max 162 :min 154)))

(defworkout "2009-01-30" "10.56"
  (:warm 6:53.8 1.0 :hr (130 :max 136))
  (:warm 6:32.0 1.0 :hr (140 :max 144))
  (:work 6:18.0 1.0 :hr (143 :max 145))
  (:work 6:14.3 1.0 :hr (146 :max 150))
  (:work 6:13.7 1.0 :hr (147 :max 150))
  (:work 6:12.1 1.0 :hr (149 :max 156))
  (:work 6:13.9 1.0 :hr (149 :max 153))
  (:cool 2:33.9 .36 :hr (144 :max 150 :min 142)))

(defworkout "2009-02-01" "12.07"
  (:warm 7:17.9 1.0 :hr (134 :max 145))
  (:warm 6:35.9 1.0 :hr (145 :max 148))
  (:work 6:20.0 1.0 :hr (149 :max 153))
  (:work 6:14.0 1.0 :hr (153 :max 159))
  (:work 6:14.0 1.0 :hr (155 :max 159))
  (:work 6:26.0 1.0 :hr (157 :max 161))
  (:work 6:16.0 1.0 :hr (158 :max 160))
  (:work 6:18.0 1.0 :hr (158 :max 162))
  (:work 6:10.3 1.0 :hr (161 :max 165))
  (:work 6:20.0 1.0 :hr (161 :max 168))
  (:work 6:09.9 1.0 :hr (157 :max 167))
  (:work 6:09.9 1.0 :hr (159 :max 162))
  (:work 6:10.3 1.0 :hr (160 :max 163))
  (:cool 6:44.0 1.0 :hr (154 :max 163))
  (:cool 0:34.4 .08 :hr (154 :max 156)))

(defworkout "2009-02-03" "17.40"
  (:warm 10:19.3 1600 :hr (148 :max 156))
  ;;pause 6:07.0
  (:warm  2:22.3  400 :hr (150 :max 167))
  (:warm  0:57.6  200 :hr (170 :max 173))
  (:warm  2:16.1  400 :hr (170 :max 173 :min 168))
  (:warm  0:52.3  200 :hr (174 :max 178))
  (:warm  2:28.3  400 :hr (170 :max 179 :min 162))
  ;;pause 7:00
  (:work  0:46.5  200 :hr (157 :max 168))
  (:work  0:51.6  200 :hr (172 :max 176))
  (:work  0:50.9  200 :hr (177 :max 179))
  (:work  0:48.1  200 :hr (179 :max 180))
  (:rest  3:01.2  200 :hr (160 :max 180 :min 131))
  (:work  1:40.8  400 :hr (163 :max 180))
  (:work  0:48.7  200 :hr (182 :max 184))
  (:work  0:48.9  200 :hr (184 :max 185))
  (:rest  2:56.7  200 :hr (166 :max 185 :min 139))
  (:work  0:49.7  200 :hr (156 :max 173))
  (:work  0:50.6  200 :hr (179 :max 183))
  (:work  0:48.8  200 :hr (185 :max 185))
  (:work  0:49.0  200 :hr (186 :max 187))
  (:rest  2:59.6    0 :hr (156 :max 186 :min 142))
  (:work  0:50.4  200 :hr (159 :max 173))
  (:work  0:51.6  200 :hr (178 :max 181))
  (:work  1:38.3  400 :hr (184 :max 185))
  (:rest  3:00.9    0 :hr (154 :max 185 :min 135))
  (:cool  5:15   1000 :hr 170)
  (:cool  6:00   1000 :hr 160))

(defworkout "2009-02-05" "16.19"
  (:warm 6:47.8 1.0 :hr (136 :max 143))
  (:warm 6:04.0 1.0 :hr (148 :max 155))
  (:warm 5:38.1 1.0 :hr (158 :max 167))
  (:work 5:16.1 1.0 :hr (166 :max 169))
  (:work 5:05.8 1.0 :hr (171 :max 175))
  (:work 5:06.3 1.0 :hr (173 :max 174))
  (:work 4:51.7 1.0 :hr (176 :max 177))
  (:cool 5:24.1 1.0 :hr (169 :max 176))
  (:cool 5:57.9 1.0 :hr (160 :max 171 :min 155))
  (:cool 2:38.0 .38 :hr (152 :max 156 :min 148)))

(defworkout "2009-02-06" "10.51"
  (:warm 6:49.8 1.0 :hr (132 :max 142))
  (:warm 6:24.0 1.0 :hr (141 :max 145))
  (:work 6:26.0 1.0 :hr (144 :max 150))
  (:work 6:28.1 1.0 :hr (146 :max 149))
  (:work 6:28.2 1.0 :hr (146 :max 150))
  (:work 6:25.8 1.0 :hr (147 :max 152))
  (:cool 6:33.7 .98 :hr (148 :max 153)))

(defworkout "2009-02-08" "10.34"
  (:warm 6:56.0 1.0 :hr (130 :max 138))
  (:warm 6:41.9 1.0 :hr (138 :max 142))
  (:work 6:25.9 1.0 :hr (145 :max 150))
  (:work 6:22.1 1.0 :hr (148 :max 153))
  (:work 6:28.0 1.0 :hr (148 :max 152))
  (:work 6:24.0 1.0 :hr (150 :max 152))
  (:work 6:36.0 1.0 :hr (150 :max 155))
  (:work 6:29.9 1.0 :hr (149 :max 154))
  (:work 6:34.3 1.0 :hr (151 :max 156))
  (:work 5:57.9 1.0 :hr (153 :max 157))
  (:work 5:53.9 1.0 :hr (155 :max 158))
  (:work 5:49.9 1.0 :hr (160 :max 165))
  (:work 5:50.0 1.0 :hr (157 :max 161))
  (:work 6:06.1 1.0 :hr (155 :max 159))
  (:cool 6:46.0 1.0 :hr (150 :max 154)))

(defworkout "2009-02-09" "10.59"
  (:warm 7:23.7 1.00 :hr (122 :max 132))
  (:warm 6:42.0 1.00 :hr (135 :max 141))
  (:work 6:24.2 1.00 :hr (143 :max 148))
  (:work 6:05.8 1.00 :hr (151 :max 163))
  (:work 1:49.6 0.29 :hr (148 :max 157))
  (:hill 0:52.0 0.20 :hr (165 :max 172))
  (:work 3:12.5 0.51 :hr (158 :max 172 :min 152))
  (:work 1:37.9 0.26 :hr (154 :max 160))
  (:hill 0:44.3 0.20 :hr (169 :max 180))
  (:work 3:23.7 0.54 :hr (160 :max 181 :min 149))
  (:work 6:16.1 1.00 :hr (152 :max 157))
  (:work 3:46.7 0.60 :hr (152 :max 155)))

(defworkout "2009-02-12" "16.38"
  (:warm 6:49.9 1.0 :hr (130 :max 135))
  (:warm 6:12.2 1.0 :hr (142 :max 146))
  (:warm 5:25.8 1.0 :hr (155 :max 162))
  (:work 4:52.0 1.0 :hr (165 :max 167))
  (:work 4:45.9 1.0 :hr (168 :max 169))
  (:work 4:36.3 1.0 :hr (171 :max 172))
  (:cool 5:41.7 1.0 :hr (159 :max 171 :min 155))
  (:cool 6:10.0 1.0 :hr (150 :max 155 :min 149))
  (:cool 2:20.6 .36 :hr (147 :max 150 :min 145)))

(defworkout "2009-02-15" "12.35"
  (:warm 10:06.2 1600 :hr (142 :max 155))
  ;;pause 7:07.0
  (:warm  2:13.7  400 :hr (156 :max 168))
  (:warm  0:26.6  100 :hr (171 :max 173))
  (:warm  1:40.2  300 :hr (169 :max 174))
  (:warm  0:27.0  100 :hr (172 :max 175))
  (:warm  1:31.9  300 :hr (174 :max 176))
  ;;pause 7:34.7
  (:warm  0:25.3  100 :hr (149 :max 158))
  (:warm  0:26.3  100 :hr (149 :max 157))
  ;; 13:13
  (:work  0:58.7  200 :hr (155 :max 165))
  (:work  0:57.3  200 :hr (169 :max 172))
  (:work  0:54.6  200 :hr (174 :max 176))
  (:work  1:47.8  400 :hr (179 :max 182))
  (:work  1:49.4  400 :hr (181 :max 182))
  (:work  1:49.5  400 :hr (182 :max 183))
  (:work  1:49.4  400 :hr (183 :max 188))
  (:work  1:50.1  400 :hr (183 :max 186))
  (:work  1:47.9  400 :hr (184 :max 186))
  (:work  1:50.6  400 :hr (184 :max 186))
  (:work  0:54.9  200 :hr (183 :max 184))
  (:work  0:54.0  200 :hr (183 :max 185))
  (:work  0:54.0  200 :hr (184 :max 186))
  (:work  0:52.9  200 :hr (185 :max 186))
  (:work  2:10.4  500 :hr (186 :max 187))
  (:work  0:47.3  200 :hr (187 :max 187))
  (:work  0:21.0  100 :hr (187 :max 187))
  ;;pause
  (:cool  6:55.9 1200 :hr (153 :max 158)))

(defworkout "2009-02-15" "13.54"
  (:work 0:02.8 20)
  (:rest 3:04.8  0)
  (:work 0:02.8 20))

(defworkout "2009-02-16" "11.46"
  (:warm 6:54.0 1.0 :hr (126 :max 134))
  (:warm 6:30.0 1.0 :hr (136 :max 142))
  (:work 6:08.0 1.0 :hr (140 :max 144))
  (:work 6:14.0 1.0 :hr (145 :max 151))
  (:work 6:11.9 1.0 :hr (146 :max 151))
  (:cool 4:01.3 .63 :hr (148 :max 151)))

(defworkout "2009-02-17" "17.02"
  (:warm 6:39.8 1.0 :hr (132 :max 141))
  (:warm 6:14.0 1.0 :hr (143 :max 146))
  (:warm 5:30.3 1.0 :hr (157 :max 166))
  (:work 4:51.7 1.0 :hr (164 :max 168))
  (:work 4:42.2 1.0 :hr (170 :max 174))
  (:work 4:39.8 1.0 :hr (171 :max 173))
  (:work 4:36.0 1.0 :hr (173 :max 175))
  (:cool 2:49.0 .52 :hr (170 :max 176 :min 165))
  (:cool 2:43.0 .48 :hr (162 :max 165 :min 159))
  (:cool 6:14.0 1.0 :hr (154 :max 160 :min 153))
  (:cool 1:02.3 .14 :hr (150 :max 153 :min 148)))

(defworkout "2009-02-19" "16.16"
  (:warm 10:07.8 1600 :hr (140 :max 150))
  ;;pause 7:54.7, :min 118
  (:warm  6:18.7 1200 :hr (163 :max 172))
  ;;pause 1:22.3, :min 124
  (:warm  0:22.2  100 :hr (144 :max 156))
  ;;pause 4:52.4, :min 131
  (:warm  0:52.3  200 :hr (154 :max 166))
  ;;pause
  ;; 16:49
  (:work  0:51.9  200 :hr (158 :max 168))
  (:work  0:53.6  200 :hr (172 :max 175))
  (:work  0:51.6  200 :hr (176 :max 178))
  (:work  0:50.3  200 :hr (178 :max 179))
  (:rest  2:56.5  200 :hr (155 :max 178 :min 128))
  (:work  0:51.8  200 :hr (151 :max 166))
  (:work  0:52.1  200 :hr (173 :max 179))
  (:work  0:51.5  200 :hr (180 :max 181))
  (:work  0:51.9  200 :hr (181 :max 182))
  (:rest  2:59.3  200 :hr (156 :max 180 :min 137))
  (:work  0:53.4  200 :hr (151 :max 164))
  (:work  0:49.5  200 :hr (174 :max 180))
  (:work  0:51.0  200 :hr (181 :max 182))
  (:work  0:50.7  200 :hr (183 :max 184))
  (:rest  3:00.6  200 :hr (162 :max 182 :min 139))
  (:work  0:53.8  200 :hr (152 :max 170))
  (:work  0:51.7  200 :hr (176 :max 180))
  (:work  0:50.2  200 :hr (182 :max 183))
  (:work  0:50.9  200 :hr (182 :max 184))
  (:rest  2:56.9    0 :hr (149 :max 182 :min 135))
  (:work  0:52.0  200 :hr (152 :max 171))
  (:work  0:50.7  200 :hr (177 :max 181))
  (:work  0:50.1  200 :hr (182 :max 183))
  (:work  0:48.6  200 :hr (184 :max 185))
  ;;pause 4:02.2, :min 117
  (:cool  4:21.5  800 :hr (156 :max 167))
  ;;pause 8:20.2, :min 117
  (:cool  6:25.4 1000 :hr (147 :max 153)))

(defworkout "2009-02-20" "10.57"
  (:warm 6:49.9 1.0 :hr (131 :max 138))
  (:warm 6:27.8 1.0 :hr (144 :max 151))
  (:work 6:40.2 1.0 :hr (142 :max 148))
  (:work 6:26.1 1.0 :hr (147 :max 151))
  (:work 6:25.9 1.0 :hr (150 :max 155))
  (:work 5:58.2 1.0 :hr (157 :max 161))
  (:cool 6:31.9 1.0 :hr (151 :max 158))
  (:cool 1:00.9 .14 :hr (147 :max 148)))

(defworkout "2009-02-22" "11.20"
  (:warm 6:51.9 1.0 :hr (131 :max 139))
  (:warm 6:31.8 1.0 :hr (142 :max 147))
  (:work 6:32.0 1.0 :hr (148 :max 152))
  (:work 6:34.3 1.0 :hr (151 :max 156))
  (:work 6:25.8 1.0 :hr (153 :max 157))
  (:work 6:24.1 1.0 :hr (155 :max 159))
  (:work 6:19.8 1.0 :hr (159 :max 163))
  (:work 6:14.2 1.0 :hr (157 :max 161))
  (:work 6:13.9 1.0 :hr (159 :max 163))
  (:work 6:13.9 1.0 :hr (158 :max 162))
  (:work 6:08.2 1.0 :hr (154 :max 162))
  (:work 6:00.0 1.0 :hr (158 :max 161))
  (:work 6:11.9 1.0 :hr (158 :max 160))
  (:cool 6:38.0 1.0 :hr (155 :max 158))
  (:cool 0:44.6 .10 :hr (154 :max 155)))

(defworkout "2009-02-23" "10.58"
  (:warm 6:47.8 1.00 :hr (128 :max 138))
  (:warm 6:24.0 1.00 :hr (143 :max 149))
  (:work 6:22.2 1.00 :hr (145 :max 151))
  (:work 6:09.8 1.00 :hr (148 :max 157))
  (:work 3:56.0 0.65 :hr (157 :max 167))
  (:hill 0:57.6 0.22 :hr (171 :max 174))
  (:work 0:48.5 0.13 :hr (171 :max 174 :min 166))
  (:work 4:13.8 0.65 :hr (152 :max 166))
  (:hill 0:54.0 0.20 :hr (170 :max 176))
  (:work 0:50.4 0.15 :hr (171 :max 176 :min 166))
  (:work 6:05.7 1.00 :hr (155 :max 166))
  (:work 5:53.8 0.94 :hr (153 :max 159)))

(defworkout "2009-02-26" "15.59"
  (:warm 6:39.9 1.0 :hr (133 :max 141))
  (:warm 6:06.3 1.0 :hr (143 :max 152))
  (:warm 5:31.9 1.0 :hr (160 :max 168))
  (:work 4:58.1 1.0 :hr (167 :max 170))
  (:work 4:55.9 1.0 :hr (171 :max 175))
  (:work 4:51.2 1.0 :hr (173 :max 174))
  (:work 4:56.0 1.0 :hr (174 :max 176))
  (:work 4:54.0 1.0 :hr (174 :max 176))
  (:work 4:56.0 1.0 :hr (175 :max 176))
  (:cool 2:45.6 .50 :hr (170 :max 176 :min 170))
  (:cool 2:42.3 .50 :hr (167 :max 170 :min 164))
  (:cool 6:00.1 1.0 :hr (160 :max 164 :min 157))
  (:cool 4:02.7 .62 :hr (153 :max 157 :min 153)))

(defworkout "2009-02-27" "11.08"
  (:warm 6:41.8 1.00 :hr (128 :max 136))
  (:warm 6:22.0 1.00 :hr (143 :max 154))
  (:work 6:02.0 1.00 :hr (150 :max 155))
  (:work 4:39.5 0.79 :hr (159 :max 167))
  (:hill 0:56.5 0.21 :hr (172 :max 174))
  (:work 4:29.4 0.75 :hr (161 :max 174))
  (:hill 0:52.9 0.20 :hr (175 :max 180))
  (:work 0:11.8 0.05 :hr (180 :max 180))
  (:work 4:25.1 0.72 :hr (162 :max 176))
  (:hill 0:47.2 0.20 :hr (175 :max 182))
  (:work 0:21.6 0.08 :hr (182 :max 183))
  (:work 5:58.2 1.00 :hr (162 :max 180 :min 154))
  (:cool 6:21.1 0.99 :hr (154 :max 162)))

(defworkout "2009-03-01" "10.58"
  (:warm 2:33.3 .34 :hr 120)
  (:warm 6:49.9 1.0 :hr (131 :max 138))
  (:warm 6:27.9 1.0 :hr (138 :max 142))
  (:work 6:32.1 1.0 :hr (144 :max 149))
  (:work 6:34.1 1.0 :hr (146 :max 152))
  (:work 7:28.0 1.0 :hr (152 :max 158))
  (:work 5:43.8 1.0 :hr (144 :max 148))
  (:work 6:08.1 1.0 :hr (150 :max 155))
  (:work 6:11.9 1.0 :hr (154 :max 162))
  (:work 6:08.1 1.0 :hr (153 :max 157))
  (:work 6:12.4 1.0 :hr (151 :max 157))
  (:work 6:07.9 1.0 :hr (152 :max 155))
  (:work 6:08.0 1.0 :hr (152 :max 155))
  (:work 5:50.0 1.0 :hr (157 :max 166))
  (:work 6:02.0 1.0 :hr (159 :max 174))
  (:work 5:52.0 1.0 :hr (155 :max 174 :min 149))
  (:cool 6:48.0 1.0 :hr (147 :max 152)))

(defworkout "2009-03-02" "11.05"
  (:warm 6:47.8 1.0 :hr (131 :max 138))
  (:warm 6:20.3 1.0 :hr (142 :max 148))
  (:work 6:17.9 1.0 :hr (146 :max 151))
  (:work 6:11.9 1.0 :hr (149 :max 154))
  (:work 6:01.9 1.0 :hr (155 :max 159))
  (:work 5:52.2 1.0 :hr (158 :max 169))
  (:work 6:14.1 1.0 :hr (158 :max 173))
  (:work 0:19.7 .04 :hr (152 :max 152)))

(defworkout "2009-03-03" "16.04"
  (:warm 6:30.1 1.00 :hr (136 :max 144))
  (:warm 6:05.8 1.00 :hr (147 :max 153))
  (:warm 5:27.9 1.00 :hr (161 :max 169))
  (:work 4:56.0 1.00 :hr (169 :max 172))
  (:work 4:52.2 1.00 :hr (172 :max 173))
  (:work 4:47.9 1.00 :hr (173 :max 175))
  (:work 4:38.0 1.00 :hr (176 :max 178))
  (:cool 2:48.0 0.50 :hr (169 :max 177 :min 166))
  (:cool 2:44.1 0.50 :hr (165 :max 167 :min 164))
  (:cool 3:09.6 0.52 :hr (161 :max 164 :min 156))
  (:cool 2:54.2 0.48 :hr (156 :max 158))
  (:cool 1:51.4 0.28 :hr (156 :max 158)))

(defworkout "2009-03-05" "16.12"
  (:warm 3:17.2  500  :hr (136 :max 145))
  (:warm 3:11.9  500  :hr (147 :max 149))
  (:warm 3:58.4  666  :hr (154 :max 157))
  ;;pause 7:40.7
  (:warm 2:38.5  500  :hr (161 :max 166))
  (:warm 2:42.5  500  :hr (163 :max 168))
  (:warm 2:42.6  500  :hr (165 :max 172))
  ;;pause
  (:work 1:30.3 1000/3 :hr (163 :max 172))
  (:work 1:29.4 1000/3 :hr (175 :max 178))
  (:work 1:25.3 1000/3 :hr (179 :max 180))
  (:rest 2:59.6  300   :hr (160 :max 180 :min 136))
  (:work 1:25.2 1000/3 :hr (162 :max 177))
  (:work 1:26.5 1000/3 :hr (180 :max 183))
  (:work 1:26.1 1000/3 :hr (183 :max 186))
  (:rest 2:59.6  300   :hr (160 :max 181 :min 136))
  (:work 1:27.6 1000/3 :hr (164 :max 177))
  (:work 1:24.9 1000/3 :hr (181 :max 183))
  (:work 1:22.9 1000/3 :hr (183 :max 188))
  (:rest 2:59.4  300   :hr (159 :max 186 :min 142))
  (:work 1:25.9 1000/3 :hr (165 :max 179))
  (:work 1:49.2 1300/3 :hr (182 :max 183))
  (:work 0:56.4  700/3 :hr (184 :max 184))
  ;;pause 5:59.1       :hr (135 :max 184 :min 115)
  (:cool 1:45.2  400   :hr (156 :max 171))
  ;;pause 3:00.6       :hr (131 :max 171 :min 119)
  (:cool 2:44.4  500   :hr (150 :max 159))
  (:cool 3:02.0  500   :hr (152 :max 158))
  (:cool 3:15.3  500   :hr (149 :max 154)))

(defworkout "2009-03-06" "11.26"
  (:warm 6:42.0 1.00 :hr (130 :max 136))
  (:warm 6:13.8 1.00 :hr (141 :max 145))
  (:work 6:04.1 1.00 :hr (144 :max 148))
  (:work 5:58.0 1.00 :hr (147 :max 151))
  (:work 3:51.2 0.63 :hr (149 :max 152))
  (:hill 1:16.2 0.21 :hr (151 :max 153))
  (:work 0:57.8 0.16 :hr (151 :max 153))
  (:work 6:11.8 1.00 :hr (148 :max 152))
  (:cool 6:30.2 1.00 :hr (148 :max 156))
  (:cool 0:12.2 0.03 :hr (146 :max 146)))

(defworkout "2009-03-08" "10.46"
  (:warm 6:43.9 1.00 :hr (131 :max 143))
  (:warm 6:24.0 1.00 :hr (140 :max 145))
  (:work 6:08.0 1.00 :hr (144 :max 151))
  (:work 6:30.1 1.00 :hr (146 :max 152))
  (:work 7:33.8 1.00 :hr (152 :max 159))
  (:work 6:02.0 1.00 :hr (148 :max 153))
  (:work 6:02.0 1.00 :hr (148 :max 154))
  (:work 6:02.2 1.00 :hr (152 :max 157))
  (:work 6:03.8 1.00 :hr (149 :max 155))
  (:work 6:08.0 1.00 :hr (153 :max 158))
  (:work 5:28.0 1.00 :hr (160 :max 164))
  (:work 5:18.3 1.00 :hr (163 :max 167))
  (:work 5:13.8 1.00 :hr (166 :max 169))
  (:work 5:36.0 1.00 :hr (171 :max 175))
  (:work 5:23.9 1.00 :hr (163 :max 175))
  (:cool 4:28.1 0.69 :hr (155 :max 165 :min 151)))

(defworkout "2009-03-09" "16.02"
  (:warm 6:37.8 1.00 :hr (132 :max 140))
  (:warm 6:18.3 1.00 :hr (141 :max 143))
  (:work 5:56.0 1.00 :hr (147 :max 152))
  (:work 4:48.8 0.76 :hr (151 :max 157))
  (:hill 0:46.7 0.16 :hr (161 :max 168))
  (:work 0:26.2 0.08 :hr (169 :max 170))
  (:work 1:31.2 0.24 :hr (155 :max 167 :min 149))
  (:hill 0:28.6 0.10 :hr (159 :max 167))
  (:work 1:58.4 0.33 :hr (160 :max 170 :min 152))
  (:hill 0:25.9 0.10 :hr (161 :max 170))
  (:work 1:25.9 0.23 :hr (168 :max 175 :min 153))
  (:work 1:11.8 0.19 :hr (147 :max 153))
  (:hill 0:24.6 0.11 :hr (160 :max 172))
  (:work 2:38.9 0.42 :hr (162 :max 177 :min 151))
  (:hill 0:23.1 0.10 :hr (160 :max 173))
  (:work 0:57.7 0.18 :hr (173 :max 177 :min 163))
  (:work 5:59.9 1.00 :hr (153 :max 163))
  (:work 6:10.0 1.00 :hr (150 :max 156))
  (:work 6:14.1 1.00 :hr (149 :max 152))
  (:cool 0:47.5 0.10 :hr (146 :max 148)))

(defworkout "2009-03-11" "11.21"
  (:warm 6:55.9 1.0 :hr (127 :max 135))
  (:warm 6:50.0 1.0 :hr (137 :max 140))
  (:work 6:43.9 1.0 :hr (141 :max 146))
  (:work 6:34.2 1.0 :hr (145 :max 150))
  (:work 6:35.9 1.0 :hr (145 :max 151))
  (:work 6:40.0 1.0 :hr (147 :max 150)))

(defworkout "2009-03-12" "16.21"
  (:work 2:03.1 0.3 :hr (135 :max 146))
  (:work 3:45.0 0.6 :hr (152 :max 158))
  (:work 1:47.4 0.3 :hr (159 :max 162))
  (:work 1:41.1 0.3 :hr (164 :max 167)))

(defworkout "2009-03-12" "17.03"
  (:warm 6:45   1.0 :hr 140)
  (:warm 5:44.0 1.0 :hr (158 :max 170))
  ;pause 3:04.9          137 :max 163
  (:work 6:46.9 1.5 :hr (177 :max 184))
  (:rest 3:00.2 0.2 :hr (163 :max 184 :min 146))
  (:work 4:23.8 1.0 :hr (176 :max 185))
  (:work 2:13.3 0.5 :hr (184 :max 185))
  (:rest 3:59.5 0.3 :hr (162 :max 184 :min 148))
  (:work 4:55.3 1.1 :hr (178 :max 184))
  ;pause 4:58.2     :hr (146 :max 184 :min 140)
  (:cool 6:20.9 1.0 :hr (160 :max 166 :min 159)))

(defworkout "2009-03-13" "11.07"
  (:warm 13 2.00 :hr 145)
  (:work 34 5.84 :hr (155 :max 175)))

(defworkout "2009-03-15" "11.18"
  (:warm 6:41.7 1.0 :hr (120 :max 133))
  (:warm 6:38.0 1.0 :hr (135 :max 147))
  (:work 6:22.3 1.0 :hr (140 :max 151))
  (:work 6:05.9 1.0 :hr (145 :max 152))
  (:work 6:11.9 1.0 :hr (147 :max 155))
  (:work 6:19.9 1.0 :hr (141 :max 153))
  (:work 6:40.0 1.0 :hr (143 :max 158))
  (:work 6:20.1 1.0 :hr (140 :max 155))
  (:work 6:26.3 1.0 :hr (148 :max 155))
  (:work 6:17.8 1.0 :hr (147 :max 154))
  (:work 6:05.9 1.0 :hr (148 :max 155))
  (:work 6:16.0 1.0 :hr (145 :max 156))
  (:work 6:44.0 1.0 :hr (148 :max 155))
  (:work 5:38.0 1.0 :hr (156 :max 160))
  (:work 6:02.0 1.0 :hr (151 :max 155))
  (:work 6:12.2 1.0 :hr (150 :max 154))
  (:work 6:31.9 1.0 :hr (141 :max 159))
  (:work 1:58.3 .27 :hr (143 :max 146)))

(defworkout "2009-03-16" "11.00"
  (:warm 6:39.7 1.00 :hr (126 :max 135))
  (:warm 6:06.3 1.00 :hr (140 :max 145))
  (:work 5:53.8 1.00 :hr (148 :max 152))
  (:work 4:25.7 0.75 :hr (152 :max 159))
  (:hill 0:52.7 0.20 :hr (166 :max 170))
  (:work 5:40.7 0.91 :hr (152 :max 171))
  (:hill 0:48.9 0.20 :hr (169 :max 177))
  (:work 5:43.7 0.90 :hr (156 :max 177))
  (:hill 0:42.5 0.20 :hr (174 :max 179))
  (:work 5:29.9 0.87 :hr (156 :max 181 :min 148))
  (:work 6:12.0 1.00 :hr (150 :max 157))
  (:work 1:04.4 0.18 :hr (152 :max 154)))

(defworkout "2009-03-17" "15.55"
  (:warm 9:22.2 1500   :hr (142 :max 149))
  ;pause 4:43.7
  (:warm 0:11.1   60   :hr (137 :max 161))
  ;pause 1:13.7
  (:warm 0:11.6   60   :hr (150 :max 160))
  ;pause 1:36.4
  (:warm 7:51.1 1500   :hr (158 :max 171))
  ;pause 6:03.4
  (:work 1:28.1 1000/3 :hr (156 :max 170))
  (:work 1:26.2 1000/3 :hr (174 :max 175))
  (:work 1:26.2 1000/3 :hr (175 :max 176))
  (:rest 2:58.6  300   :hr (153 :max 176 :min 125))
  (:work 1:27.1 1000/3 :hr (160 :max 171))
  (:work 1:24.1 1000/3 :hr (177 :max 179))
  (:work 1:22.9 1000/3 :hr (179 :max 181))
  (:rest 2:58.8  300   :hr (157 :max 180 :min 121))
  (:work 1:26.0 1000/3 :hr (161 :max 173))
  (:work 1:23.4 1000/3 :hr (179 :max 181))
  (:work 1:16.7 1000/3 :hr (182 :max 183))
  (:rest 2:00.9  300   :hr (157 :max 183 :min 122))
  ;pause 4:31.8        :hr (117 :max 122 :min 112)
  (:cool 5:20.9 1000   :hr (152 :max 159))
  ;;pause 3:02.5       :hr (126 :max 157 :min 108)
  (:cool 6:05.3 1000   :hr (144 :max 150)))

(defworkout "2009-03-19" "16.22"
  (:warm 6:38.0 1.0 :hr (132 :max 138))
  (:warm 6:05.8 1.0 :hr (141 :max 147))
  (:warm 5:16.1 1.0 :hr (153 :max 162))
  (:work 4:47.9 1.0 :hr (163 :max 166))
  (:work 4:40.3 1.0 :hr (168 :max 171))
  (:work 4:29.7 1.0 :hr (173 :max 175))
  (:cool 5:26.0 1.0 :hr (164 :max 174 :min 159))
  (:cool 6:00.2 1.0 :hr (153 :max 159 :min 147))
  (:cool 2:49.9 .43 :hr (146 :max 150 :min 143)))

(defworkout "2009-03-21" "12.04"
  (:warm 10:10.6 1600 :hr (143 :max 156))
  ;pause  9:51.7    0 :hr (122 :max 152)
  (:warm  0:11.5   60 :hr 139)
  ;pause  2:29.2    0 :hr (135 :max 161)
  (:warm  1:30.4  300 :hr (154 :max 164))
  (:warm  0:24.7  100 :hr (168 :max 170))
  (:warm  1:37.2  300 :hr (168 :max 172 :min 166))
  (:warm  0:25.4  100 :hr (170 :max 172))
  (:warm  1:37.2  300 :hr (169 :max 172 :min 165))
  (:warm  0:24.9  100 :hr (169 :max 173))
  ;pause  1:30.3    0 :hr (154 :min 132)
  (:warm  0:52.0  200 :hr (153 :max 167))
  ;pause
  (:warm  0:53.4  200 :hr (156 :max 162))
  ;pause
  (:warm  0:52.8  200 :hr (129 :max 162))
  ;pause
  ;12:43
  (:work  0:51.7  200 :hr (155 :max 169))
  (:work  1:46.8  400 :hr (172 :max 177))
  (:work  1:42.3  400 :hr (179 :max 182))
  (:work  1:43.9  400 :hr (181 :max 182))
  (:work  1:46.3  400 :hr (181 :max 182))
  (:work  0:52.5  200 :hr (182 :max 183))
  (:work  0:52.8  200 :hr (182 :max 182))
  (:work  1:47.8  400 :hr (181 :max 183))
  (:work  1:46.3  400 :hr (183 :max 184))
  (:work  1:47.0  400 :hr (183 :max 185))
  (:work  1:44.8  400 :hr (182 :max 183))
  (:work  0:52.7  200 :hr (183 :max 184))
  (:work  0:52.6  200 :hr (184 :max 186))
  (:work  1:46.5  400 :hr (183 :max 185))
  (:work  0:50.4  200 :hr (184 :max 186))
  (:work  0:23.8  100 :hr (184 :max 185))
  (:work  0:20.2  100 :hr (185 :max 186))
  ;pause  9:21.6  400 :hr (129 :max 186 :min 116)
  (:cool  9:35.9 1600 :hr (145 :max 153)))

(defworkout "2009-03-23" "10.56"
  (:warm 6:49.3 1.05 :hr (128 :max 133))
  (:warm 6:35.8 1.05 :hr (139 :max 143))
  (:work 6:40.2 1.20 :hr (150 :max 154))
  (:work 6:31.6 1.15 :hr (152 :max 157))
  (:work 6:52.6 1.20 :hr (153 :max 157))
  (:work 6:04.4 1.05 :hr (153 :max 156))
  (:work 5:50.4 1.05 :hr (155 :max 159))
  (:cool 3:47.2 0.60 :hr (147 :max 155 :min 143)))

(defworkout "2009-03-26" "16.15"
  (:warm 3:15    500   :hr 130)
  (:warm 6:30   1000   :hr 135)
  ;pause
  (:warm 5:00   1000   :hr 170)
  ;pause
  (:work 0:42.2 1000/6 :hr (146 :max 159))
  (:work 0:43.6 1000/6 :hr (164 :max 167))
  (:work 1:29.7 1000/3 :hr (169 :max 171))
  (:work 1:25.2 1000/3 :hr (173 :max 175))
  (:work 0:27.5  100   :hr (176 :max 176))
  (:rest 3:29.3  300   :hr (153 :max 176 :min 126))
  (:work 0:24.5  100   :hr (141 :max 157))
  (:work 1:25.5 1000/3 :hr (170 :max 175))
  (:work 1:24.4 1000/3 :hr (177 :max 178))
  (:work 1:24.0 1000/3 :hr (179 :max 180))
  (:rest 3:29.4  300   :hr (154 :max 180 :min 116))
  (:work 0:24.6  100   :hr (134 :max 152))
  (:work 1:24.3 1000/3 :hr (170 :max 178))
  (:work 1:23.4 1000/3 :hr (179 :max 181))
  (:work 1:23.2 1000/3 :hr (181 :max 182))
  (:rest 3:37.1  300   :hr (158 :max 182 :min 125))
  (:work 0:26.6  100   :hr (137 :max 153))
  (:work 1:23.8 1000/3 :hr (169 :max 179))
  (:work 1:22.8 1000/3 :hr (181 :max 182))
  (:work 1:21.1 1000/3 :hr (183 :max 183))
  ;pause 4:06.7        :hr (135 :max 183 :min 116)
  (:cool 1:55.5 1000/3 :hr (141 :max 156))
  (:cool 1:38.8 1000/3 :hr (162 :max 167))
  (:cool 1:59.4 1000/3 :hr (159 :max 168 :min 154)))

(defworkout "2009-03-28" "11.21"
  (:warm 6:39.8 1.0 :hr (128 :max 137))
  (:warm 6:12.1 1.0 :hr (140 :max 144))
  (:work 6:02.0 1.0 :hr (144 :max 148))
  (:work 5:54.1 1.0 :hr (146 :max 149))
  (:work 6:07.9 1.0 :hr (150 :max 157))
  (:work 6:11.8 1.0 :hr (152 :max 157))
  (:work 6:00.1 1.0 :hr (153 :max 161))
  (:work 5:54.1 1.0 :hr (156 :max 158))
  (:work 5:57.9 1.0 :hr (156 :max 158))
  (:work 5:32.1 1.0 :hr (161 :max 168))
  (:cool 5:51.8 1.0 :hr (160 :max 169))
  (:cool 2:54.7 .47 :hr (153 :max 156)))

(defworkout "2009-03-30" "11.11"
  (:warm 6:45.8 1.0 :hr (129 :max 135))  ;6:35.50 (Garmin Forerunner 305)
  (:warm 6:20.0 1.0 :hr (140 :max 143))  ;6:13.39
  (:work 5:54.0 1.0 :hr (149 :max 154))  ;5:41.59
  (:work 5:36.0 1.0 :hr (153 :max 158))  ;5:36.16
  (:work 5:48.2 1.0 :hr (150 :max 152))  ;5:40.82
  (:work 5:49.8 1.0 :hr (152 :max 158))  ;5:41.72
  (:work 6:00.1 1.0 :hr (148 :max 152))  ;5:55.56
  (:cool 1:09.3 .18 :hr (145 :max 147))) ;1:58.50, 0.321km

(defworkout "2009-03-31" "15.36"
  (:warm 6:45.9 1.0 :hr (130 :max 135))  	  ;6:18.78 (FR305)
  (:warm 6:06.1 1.0 :hr (141 :max 148))  	  ;5:59.04
  (:warm 5:15.9 1.0 :hr (157 :max 165))  	  ;5:14.83
  (:work 4:41.9 1.0 :hr (167 :max 169))  	  ;4:39.52
  (:work 4:40.0 1.0 :hr (171 :max 174))  	  ;4:39.87
  (:work 4:38.1 1.0 :hr (173 :max 176))  	  ;4:35.11
  (:work 4:34.1 1.0 :hr (176 :max 178))  	  ;4:28.08+0:50.86, 1.186km
  (:cool 5:31.8 1.0 :hr (164 :max 175 :min 158))  ;5:28.17
  (:cool 4:10.8 .67 :hr (153 :max 159 :min 148))) ;4:10.01, 0.696km

(defworkout "2009-04-03" "11.14"
  (:warm 6:41.7 1.0 :hr (125 :max 134))  ;6:28.70 (FR305, 1.01km laps)
  (:warm 6:14.0 1.0 :hr (136 :max 140))  ;6:05.26
  (:work 5:46.0 1.0 :hr (143 :max 147))  ;5:49.85
  (:work 5:54.2 1.0 :hr (144 :max 147))  ;5:55.32
  (:work 5:44.0 1.0 :hr (145 :max 148))  ;5:40.69
  (:work 5:44.0 1.0 :hr (144 :max 147))  ;5:45.23
  (:work 5:49.8 1.0 :hr (144 :max 147))  ;5:47.33
  (:work 5:58.3 1.0 :hr (145 :max 149))  ;5:54.48
  (:work 0:09.9 .02 :hr (145 :max 145))) ;0:35.26, 0.104km

(defworkout "2009-04-05" "10.38"
  (:warm 6:26.0 1.0 :hr (126 :max 137))  ;6:17.21 (FR305, 1.01km laps)
  (:warm 6:17.9 1.0 :hr (136 :max 147))  ;5:55.69
  (:work 6:12.0 1.0 :hr (141 :max 146))  ;6:03.31
  (:work 5:52.0 1.0 :hr (144 :max 150))  ;5:53.07
  (:work 5:58.0 1.0 :hr (147 :max 156))  ;5:52.08
  (:work 6:00.2 1.0 :hr (141 :max 155))  ;5:59.96
  (:work 5:57.8 1.0 :hr (148 :max 154))  ;5:55.36
  (:work 6:02.1 1.0 :hr (148 :max 152))  ;5:55.21
  (:work 5:58.0 1.0 :hr (146 :max 153))  ;6:01.63
  (:work 6:18.2 1.0 :hr (146 :max 150))  ;5:57.49
  (:work 5:57.8 1.0 :hr (147 :max 152))  ;6:15.59
  (:work 6:02.2 1.0 :hr (145 :max 152))  ;5:54.66
  (:work 5:40.0 1.0 :hr (148 :max 157))  ;5:41.25
  (:work 5:34.0 1.0 :hr (158 :max 164))  ;5:32.63
  (:work 5:22.0 1.0 :hr (158 :max 163))  ;5:26.22
  (:work 5:30.0 1.0 :hr (160 :max 165))  ;5:24.49
  (:work 2:44.6 .49 :hr (157 :max 162))  ;5:45.37
  (:cool 3:09.7 .51 :hr (139 :max 162))  ;
  (:cool 3:34.6 .53 :hr (141 :max 147))) ;4:45.38, 0.733km

(defworkout "2009-04-07" "16.02"
  (:warm 9:17.0 1500 :hr (142 :max 150))
  ;pause 6:47.7
  (:warm 7:34.0 1500 :hr (166 :max 175))
  ;pause 5:00
  ;;16:31
  (:work 1:27.7  333 :hr (155 :max 174))
  (:work 1:24.0  333 :hr (176 :max 179))
  (:work 1:25.6  333 :hr (179 :max 180))
  (:work 0:25.4  100 :hr (179 :max 179))
  (:rest 3:30.5  300 :hr (153 :max 179 :min 124))
  (:work 1:23.3  333 :hr (160 :max 177))
  (:work 1:25.6  333 :hr (179 :max 182))
  (:work 1:20.8  333 :hr (181 :max 184))
  (:work 0:24.8  100 :hr (182 :max 183))
  (:rest 3:29.5  300 :hr (155 :max 181 :min 126))
  (:work 1:21.5  333 :hr (155 :max 177))
  (:work 1:23.3  333 :hr (179 :max 182))
  (:work 1:22.3  333 :hr (181 :max 183))
  (:work 0:24.5  100 :hr (181 :max 182))
  (:rest 3:30.1  300 :hr (157 :max 182 :min 128))
  (:work 1:24.6  333 :hr (159 :max 178))
  (:work 1:21.8  333 :hr (181 :max 183))
  (:work 1:18.7  333 :hr (183 :max 185))
  (:work 0:20.9  100 :hr (184 :max 185))
  (:rest 3:37.9  300 :hr (156 :max 185 :min 129))
  ;pause 4:39.7
  (:cool 5:00.3 1000 :hr (160 :max 169))
  ;pause 3:16.7
  (:cool 6:12.7 1000 :hr (140 :max 147)))
  
(defworkout "2009-04-09" "12.35"
  (:warm 9:40.9 1500   :hr (134 :max 146))
  ;pause 18:04.6       :hr (134 :max 159)
  (:warm 7:34.1 1500   :hr (165 :max 174))
  ;pause 7:10          :hr (123 :max 168)
  ;;13:18
  (:work 0:45.6  333/2 :hr (147 :max 159))
  (:work 0:44.8  333/2 :hr (166 :max 170))
  (:work 0:46.2  333/2 :hr (171 :max 172))
  (:work 0:48.7  333/2 :hr (173 :max 173))
  (:work 0:49.3  333/2 :hr (172 :max 173))
  (:work 0:47.2  333/2 :hr (172 :max 173))
  (:work 0:44.6  333/2 :hr (175 :max 175))
  (:work 0:45.4  333/2 :hr (176 :max 177))
  (:work 0:42.2  333/2 :hr (178 :max 179))
  (:work 0:48.4  333/2 :hr (178 :max 179))
  (:work 0:48.2  333/2 :hr (177 :max 178))
  (:work 0:49.3  333/2 :hr (174 :max 175))
  (:work 0:44.6  333/2 :hr (175 :max 177))
  (:work 0:45.9  333/2 :hr (177 :max 179))
  (:work 0:44.8  333/2 :hr (178 :max 179))
  (:work 0:51.4  333/2 :hr (176 :max 179))
  (:work 0:48.0  333/2 :hr (173 :max 174))
  (:work 0:48.0  333/2 :hr (175 :max 176))
  (:work 0:45.3  333/2 :hr (174 :max 177))
  (:work 0:43.8  333/2 :hr (177 :max 178))
  (:work 0:44.4  333/2 :hr (179 :max 179))
  (:work 0:48.6  333/2 :hr (177 :max 179))
  (:work 0:48.9  333/2 :hr (174 :max 176))
  (:work 0:50.3  333/2 :hr (173 :max 174))
  ;pause 6:12.3             125      173 :min 112
  (:cool 5:04.4 1000   :hr (159 :max 166))
  ;pause 3:00.6             129      163 :min 121
  (:cool 5:50.7 1000   :hr (148 :max 153)))

(defworkout "2009-04-10" "08.28"
  (:warm 6:36.14 1.0 :hr (125 :max 134))
  (:warm 5:59.87 1.0 :hr (138 :max 143))
  (:work 5:35.05 1.0 :hr (145 :max 149))
  (:work 5:37.97 1.0 :hr (147 :max 150))
  (:work 5:43.76 1.0 :hr (149 :max 153))
  (:work 5:41.66 1.0 :hr (150 :max 154))
  (:work 5:44.55 1.0 :hr (151 :max 153))
  (:work 5:35.63 1.0 :hr (150 :max 152))
  (:work 4:26.54 .73 :hr (144 :max 147)))

(defworkout "2009-04-12" "11.31"
  (:warm 6:30.70 1.0 :hr (127 :max 146))
  (:warm 6:16.32 1.0 :hr (137 :max 145))
  (:work 5:58.58 1.0 :hr (145 :max 149))
  (:work 6:04.36 1.0 :hr (145 :max 148))
  (:work 6:48.09 1.0 :hr (149 :max 156))
  (:work 5:54.31 1.0 :hr (151 :max 162))
  (:work 5:25.88 1.0 :hr (150 :max 154))
  (:work 5:58.69 1.0 :hr (152 :max 156))
  (:work 5:50.19 1.0 :hr (154 :max 159))
  (:work 5:56.31 1.0 :hr (152 :max 156))
  (:work 5:51.30 1.0 :hr (153 :max 158))
  (:work 5:55.88 1.0 :hr (152 :max 154))
  (:work 5:57.45 1.0 :hr (152 :max 155))
  (:work 5:51.56 1.0 :hr (152 :max 157))
  (:work 5:50.76 1.0 :hr (153 :max 157))
  (:work 5:45.39 1.0 :hr (154 :max 158))
  (:work 5:23.33 1.0 :hr (168 :max 176))
  (:work 5:59.48 1.0 :hr (155 :max 167))
  (:cool 1:23.64 .20 :hr (150 :max 152)))

(defworkout "2009-04-14" "15.52"
  (:warm 9:35.0 1500 :hr (134 :max 145))
  ;pause 13:26.5          130      153
  (:warm 5:03.3 1000 :hr (160 :max 170))
  ;pause 5:30             115      162
  ;;16:26
  (:work 1:37.8  333 :hr (153 :max 166))
  (:work 1:41.0  333 :hr (165 :max 166))
  (:work 1:39.7  333 :hr (166 :max 168))
  (:work 1:36.2  333 :hr (169 :max 170))
  (:work 1:35.9  333 :hr (171 :max 173))
  (:work 1:35.0  333 :hr (172 :max 176))
  (:work 1:34.1  333 :hr (175 :max 177))
  (:work 1:35.0  333 :hr (174 :max 175))
  (:work 1:34.7  333 :hr (175 :max 176))
  (:work 1:32.4  333 :hr (174 :max 176))
  (:work 1:32.1  333 :hr (176 :max 178))
  (:work 1:30.7  333 :hr (176 :max 177))
  ;pause 3:50.9           130 :min 118
  (:cool 2:36.3  500 :hr (152 :max 163))
  (:cool 2:48.0  500 :hr (157 :max 159))
  (:cool 3:07.2  500 :hr (150 :max 158 :min 145)))

(defworkout "2009-04-16" "11.14"
  (:warm 6:41.87 1.0 :hr (129 :max 137))
  (:warm 6:20.59 1.0 :hr (139 :max 144))
  (:work 6:07.31 1.0 :hr (144 :max 147))
  (:work 6:07.80 1.0 :hr (145 :max 150))
  (:work 5:55.91 1.0 :hr (148 :max 163))
  (:work 5:22.90 1.0 :hr (156 :max 173))
  (:work 6:00.48 1.0 :hr (149 :max 152))
  (:cool 2:25.56 .39 :hr (146 :max 152)))

(defworkout "2009-04-18" "11.00"
  (:warm 7:00   1.2  :hr 150)
  (:work 4:54.3 1.0  :hr (178 :max 180))
  (:work 8:56.2 2.0  :hr (180 :max 182))
  (:work 4:34.1 1.0  :hr (179 :max 180))
  (:work 4:21.8 1.0  :hr (181 :max 182))
  (:work 4:20.3 1.0  :hr (181 :max 183))
  (:work 4:26.5 1.05 :hr (183 :max 185))
  (:work 1:52.9 0.45 :hr (186 :max 189))
  ;pause 3:00.4 146/188\122
  (:cool 3:28.8 0.6  :hr (145 :max 151)))

(defworkout "2009-04-21" "15.28"
  (:warm 6:39.64 1.0 :hr (133 :max 143))
  (:warm 6:12.20 1.0 :hr (143 :max 151))
  (:warm 5:30.87 1.0 :hr (151 :max 157))
  (:work 5:00.59 1.0 :hr (163 :max 166))
  (:work 5:04.11 1.0 :hr (164 :max 166))
  (:work 4:55.39 1.0 :hr (168 :max 172))
  (:work 4:48.88 1.0 :hr (170 :max 173))
  (:work 4:43.04 1.0 :hr (173 :max 176))
  (:work 4:45.67 1.0 :hr (176 :max 178))
  (:cool 5:17.70 1.0 :hr (167 :max 175))
  (:cool 4:17.98 .72 :hr (155 :max 164)))

(defworkout "2009-04-23" "15.02"
  (:warm 9:32.9 1500 :hr (136 :max 146))
  ;pause 11:53.9          130      152
  (:warm 2:51.0  500 :hr (147 :max 158))
  (:warm 2:26.2  500 :hr (165 :max 172))
  (:warm 2:42.2  500 :hr (167 :max 173))
  ;pause 5:20             121      158
  ;;15:37
  (:work 1:26.4  333 :hr (152 :max 172))
  (:work 1:26.6  333 :hr (175 :max 178))
  (:work 1:25.0  333 :hr (177 :max 179))
  (:rest 2:59.2  333 :hr (160 :max 178 :min 128))
  (:work 1:25.2  333 :hr (159 :max 175))
  (:work 1:24.5  333 :hr (177 :max 178))
  (:work 1:23.2  333 :hr (180 :max 181))
  (:rest 2:59.4  333 :hr (163 :max 180 :min 131))
  (:work 1:23.9  333 :hr (160 :max 176))
  (:work 1:21.7  333 :hr (180 :max 182))
  (:work 1:23.1  333 :hr (181 :max 183))
  (:rest 2:59.8  333 :hr (164 :max 182 :min 135))
  (:work 1:22.7  333 :hr (162 :max 178))
  (:work 1:21.7  333 :hr (181 :max 182))
  (:work 1:20.6  333 :hr (183 :max 184))
  (:rest 3:00.5  333 :hr (165 :max 184 :min 138))
  (:work 1:23.0  333 :hr (163 :max 179))
  (:work 1:21.5  333 :hr (181 :max 183))
  (:work 1:15.0  333 :hr (185 :max 187))
  (:rest 3:00.9  333 :hr (165 :max 185 :min 133))
  ;pause 4:03.6           121/133\118
  (:cool 5:23.3 1000 :hr (156 :max 166))
  ;pause 5:21.7           128/161\115
  (:cool 6:21.2 1000 :hr (144 :max 149)))

(defworkout "2009-04-24" "11.05"
  (:warm 6:37.67 1.00 :hr (130 :max 140))
  (:warm 6:17.49 1.00 :hr (143 :max 148))
  (:work 6:00.98 1.00 :hr (150 :max 155))
  (:work 6:06.92 1.00 :hr (152 :max 155))
  (:work 6:06.08 1.00 :hr (151 :max 156))
  (:work 6:05.88 1.00 :hr (151 :max 156))
  (:work 6:07.68 1.00 :hr (150 :max 154))
  (:cool 4:50.88 0.76 :hr (149 :max 155)))

(defworkout "2009-04-26" "13.13"
  (:warm 6:33.94 1.00 :hr (135 :max 144))
  (:warm 6:16.93 1.00 :hr (143 :max 147))
  (:work 6:02.82 1.00 :hr (148 :max 153))
  (:work 6:05.69 1.00 :hr (150 :max 158))
  (:work 6:56.76 1.00 :hr (156 :max 163))
  (:work 5:44.79 1.00 :hr (153 :max 157))
  (:work 5:39.68 1.00 :hr (155 :max 160))
  (:work 5:58.51 1.00 :hr (155 :max 160))
  (:work 6:03.77 1.00 :hr (153 :max 157))
  (:work 5:28.16 1.00 :hr (161 :max 168))
  (:work 5:06.93 1.00 :hr (170 :max 172))
  (:work 4:51.28 1.00 :hr (171 :max 173))
  (:work 4:58.02 1.00 :hr (173 :max 175))
  (:work 4:52.43 1.00 :hr (172 :max 174))
  (:work 4:51.85 1.00 :hr (173 :max 176))
  (:work 5:44.19 1.00 :hr (165 :max 174))
  (:work 5:21.68 1.00 :hr (162 :max 168))
  (:work 4:16.10 0.67 :hr (153 :max 162)))

(defworkout "2009-04-28" "15.34"
  (:warm 6:39.17 1.00 :hr (134 :max 142))
  (:warm 5:43.86 1.00 :hr (151 :max 164))
  (:work 4:57.95 1.00 :hr (166 :max 170))
  (:work 4:55.73 1.00 :hr (171 :max 174))
  (:work 4:52.25 1.00 :hr (173 :max 175))
  (:work 4:58.49 1.00 :hr (174 :max 176))
  (:work 4:54.23 1.00 :hr (174 :max 176))
  (:work 4:55.93 1.00 :hr (173 :max 175))
  (:work 5:01.34 1.00 :hr (173 :max 175))
  (:work 5:01.82 1.00 :hr (172 :max 173))
  (:cool 5:47.53 1.00 :hr (164 :max 172))
  (:cool 1:59.92 0.31 :hr (159 :max 162)))

(defworkout "2009-04-30" "10.31"
  (:warm 6:43.28 1.00 :hr (130 :max 135))
  (:warm 6:26.72 1.00 :hr (142 :max 146))
  (:work 6:15.81 1.00 :hr (147 :max 155))
  (:work 6:08.63 1.00 :hr (151 :max 156))
  (:work 6:08.21 1.00 :hr (152 :max 156))
  (:work 5:57.15 1.00 :hr (156 :max 170))
  (:work 6:07.43 1.00 :hr (152 :max 155))
  (:cool 5:57.10 .957 :hr (152 :max 162)))

(defworkout "2009-05-01" "15.27"
  (:warm 6:32.19 1.000 :hr (137 :max 144))
  (:warm 6:19.41 1.000 :hr (145 :max 149))
  (:work 6:03.99 1.000 :hr (150 :max 155))
  (:work 0:08.30 0.252 :hr (155 :max 155))
  (:hill 0:48.53 0.152 :hr (162 :max 168))
  (:work 1:27.59 0.230 :hr (155 :max 168)) ;min?
  (:hill 0:45.78 0.159 :hr (159 :max 169))
  (:work 1:29.18 0.235 :hr (156 :max 169)) ;min?
  (:hill 0:40.90 0.154 :hr (162 :max 171))
  (:work 5:55.94 1.000 :hr (154 :max 173))
  (:work 5:48.46 1.000 :hr (154 :max 158))
  (:work 6:07.04 1.000 :hr (148 :max 153))
  (:work 0:32.33 0.088 :hr (134 :max 137))
  (:hill 0:47.83 0.160 :hr (152 :max 163))
  (:work 1:28.90 0.234 :hr (154 :max 165)) ;min?
  (:hill 0:43.55 0.155 :hr (161 :max 171))
  (:work 1:36.67 0.258 :hr (157 :max 172)) ;min?
  (:hill 0:35.58 0.168 :hr (168 :max 178))
  (:work 1:16.95 0.218 :hr (167 :max 178)) ;min?
  (:work 6:03.26 1.000 :hr (150 :max 153))
  (:work 6:05.15 1.000 :hr (151 :max 157))
  (:cool 6:08.52 0.901 :hr (144 :max 149)))

(defworkout "2009-05-03" "14.34"
  (:warm 6:45.85 1.00 :hr (134 :max 141))
  (:warm 6:15.71 1.00 :hr (142 :max 149))
  (:warm 5:45.08 1.00 :hr (152 :max 162))
  (:work 5:03.67 1.00 :hr (169 :max 172))
  (:work 5:51.78 1.00 :hr (172 :max 175))
  (:work 4:48.82 1.00 :hr (167 :max 173))
  (:rest 5:36.36 1.00 :hr (157 :max 167))
  (:work 4:52.46 1.00 :hr (171 :max 175))
  (:work 5:06.79 1.00 :hr (171 :max 173))
  (:work 5:04.72 1.00 :hr (172 :max 174))
  (:rest 5:56.61 1.00 :hr (161 :max 175))
  (:work 4:56.67 1.00 :hr (171 :max 175))
  (:work 4:59.70 1.00 :hr (173 :max 176))
  (:work 4:55.70 1.00 :hr (173 :max 176))
  (:rest 5:54.46 1.00 :hr (161 :max 173))
  (:work 4:54.30 1.00 :hr (170 :max 173))
  (:work 4:53.76 1.00 :hr (174 :max 177))
  (:work 4:47.37 1.00 :hr (175 :max 177))
  (:cool 5:55.71 1.00 :hr (160 :max 175))
  (:cool 6:19.50 1.00 :hr (151 :max 156))
  (:cool 0:16.03 .039 :hr (145 :max 146)))

(defworkout "2009-05-05" "13.45"
  (:warm 9:31.2 1500 :hr (146 :max 159))
  ;pause 14:03.0 139/162
  (:warm 8:08.3 1500 :hr (165 :max 180))
  ;pause 4:50.7 127/169
  ;14:23
  (:work 1:28.1  333 :hr (157 :max 177))
  (:work 1:17.7  300 :hr (178 :max 180))
  (:rest 2:30.5  366 :hr (166 :max 179 :min 158))
  (:work 1:25.4  333 :hr (172 :max 180))
  (:work 1:17.6  300 :hr (182 :max 183))
  (:rest 2:30.8  366 :hr (171 :max 184 :min 163))
  (:work 1:25.8  333 :hr (171 :max 177))
  (:work 1:17.2  300 :hr (181 :max 183))
  (:rest 2:31.1  366 :hr (170 :max 181 :min 162))
  (:work 1:25.9  333 :hr (170 :max 178))
  (:work 1:25.3  333 :hr (179 :max 181))
  (:rest 2:29.5  333 :hr (168 :max 181 :min 160))
  (:work 1:23.3  333 :hr (169 :max 180))
  (:work 1:15.3  300 :hr (180 :max 182))
  (:rest 2:29.6  366 :hr (170 :max 182 :min 162))
  (:work 1:22.7  333 :hr (171 :max 182))
  (:work 1:15.6  300 :hr (181 :max 183))
  (:rest 2:30.3  366 :hr 168)
  (:work 0:41.3  333/2 :hr (170 :max 182))
  (:work 0:40.2  333/2 :hr (178 :max 183))
  (:work 1:12.8  300 :hr (182 :max 184))
  ;pause 6:27.4 130/182\126
  (:cool 5:19.9 1000 :hr (157 :max 164))
  ;pause 2:54.0 134/160\124
  (:cool 6:18.9 1000 :hr (146 :max 153 :min 145)))

(defworkout "2009-05-07" "11.52"
  (:warm 6:38.88 1.00 :hr (131 :max 139))
  (:warm 6:14.12 1.00 :hr (142 :max 146))
  (:work 5:50.42 1.00 :hr (149 :max 153))
  (:work 6:03.30 1.00 :hr (153 :max 161))
  (:work 5:42.98 1.00 :hr (152 :max 156))
  (:work 5:53.62 1.00 :hr (153 :max 158))
  (:work 5:50.62 1.00 :hr (153 :max 156))
  (:cool 2:48.84 .419 :hr (150 :max 155)))

(defworkout "2009-05-08" "11.32"
  (:warm 6:48.98 1.00 :hr (131 :max 137))
  (:warm 6:14.39 1.00 :hr (142 :max 151))
  (:warm 5:30.43 1.00 :hr (154 :max 165))
  (:work 5:00.13 1.00 :hr (166 :max 171))
  (:work 4:56.88 1.00 :hr (170 :max 173))
  (:work 4:52.01 1.00 :hr (171 :max 173))
  (:work 4:46.96 1.00 :hr (174 :max 176))
  (:work 4:47.83 1.00 :hr (175 :max 177))
  (:cool 5:29.67 1.00 :hr (160 :max 174 :min 156))
  (:cool 3:08.54 .477 :hr (150 :max 156 :min 144)))

(defworkout "2009-05-10" "11.39"
  (:warm 6:48.39 1.00 :hr (130 :max 139))
  (:warm 6:20.57 1.00 :hr (142 :max 148))
  (:work 5:58.06 1.00 :hr (148 :max 152))
  (:work 5:55.95 1.00 :hr (149 :max 153))
  (:work 6:02.46 1.00 :hr (147 :max 149))
  (:work 5:40.04 1.00 :hr (149 :max 152))
  (:work 5:38.48 1.00 :hr (151 :max 155))
  (:work 5:36.84 1.00 :hr (154 :max 158))
  (:work 4:54.70 1.00 :hr (165 :max 172))
  (:work 5:48.27 1.00 :hr (155 :max 169))
  (:cool 6:25.82 1.00 :hr (145 :max 151))
  (:cool 0:41.48 .086 :hr (140 :max 141)))

(defworkout "2009-05-12" "14.11"
  (:warm 6:49.03 1.00 :hr (133 :max 140))
  (:warm 6:14.57 1.00 :hr (145 :max 154))
  (:warm 2:40.77 .495 :hr (154 :max 159))
  (:work 5:06.14 1.00 :hr (167 :max 172))
  (:work 4:59.95 1.00 :hr (171 :max 173))
  (:work 5:02.91 1.00 :hr (172 :max 174))
  (:cool 5:46.71 1.00 :hr (161 :max 173))
  (:cool 3:40.49 .547 :hr (149 :max 156)))

(defworkout "2009-05-14" "11.25"
  (:warm 6:56.09 1.00 :hr (125 :max 137))
  (:warm 6:18.51 1.00 :hr (138 :max 142))
  (:work 5:50.25 1.00 :hr (149 :max 163))
  (:work 5:54.02 1.00 :hr (153 :max 165))
  (:cool 1:06.07 .166 :hr (143 :max 146)))

;; 5km: 25:53		25:24
;; 10km: 51:28		51:29
;; 15km: 1:17:55	1:17:56
;; 20km:		1:44:38
;; 21.1km: 1:50:13
(defworkout "2009-05-16" "14.37"
  (:work 4:54.3 1.0 :hr (168 :max 175)) ;4:53.3
  (:work 5:10.2 1.0 :hr (172 :max 175))
  (:work 5:23.6 1.0 :hr (171 :max 176))
  (:work 4:54.2 1.0 :hr (171 :max 175))
  (:work 5:31.7 1.0 :hr (174 :max 178)) ;5:27.2
  (:work 5:13.5 1.0 :hr (173 :max 176)) ;5:17.9
  (:work 4:47.9 1.0 :hr (172 :max 176))
  (:work 5:08.5 1.0 :hr (174 :max 177))
  (:work 5:12.0 1.0 :hr (172 :max 176))
  (:work 5:13.1 1.0 :hr (174 :max 178)) ;5:13.8
  (:work 5:08.4 1.0 :hr (174 :max 175))
  (:work 5:05.7 1.0 :hr (175 :max 177))
  (:work 5:14.0 1.0 :hr (174 :max 177))
  (:work 5:48.4 1.0 :hr (174 :max 176))
  (:work 5:10.5 1.0 :hr (171 :max 176)) ;5:09.8
  (:work 5:13.1 1.0 :hr (173 :max 176))
  (:work 5:18.7 1.0 :hr (172 :max 175))
  (:work 5:21.8 1.0 :hr (173 :max 176))
  (:work 5:19.6 1.0 :hr (173 :max 176))
  (:work 5:28.8 1.0 :hr (173 :max 177)) ;5:29.4
  (:work 5:35.0 1.0975 :hr (175 :max 179)))

(defworkout "2009-05-18" "12.35"
  (:warm 6:45.19 1.00 :hr (130 :max 142))
  (:warm 6:19.41 1.00 :hr (141 :max 145))
  (:work 6:03.97 1.00 :hr (146 :max 152))
  (:work 5:51.78 1.00 :hr (147 :max 151))
  (:work 5:59.01 1.00 :hr (148 :max 151))
  (:work 6:03.60 1.00 :hr (148 :max 151))
  (:work 6:00.81 1.00 :hr (149 :max 154))
  (:cool 2:13.70 .339 :hr (144 :max 152 :min 139 :end 141)))

(defworkout "2009-05-20" "11.30"
  (:warm 6:37.12 1.00 :hr (126 :max 135))
  (:warm 6:15.43 1.00 :hr (137 :max 142))
  (:work 5:52.14 1.00 :hr (144 :max 146))
  (:work 5:57.61 1.00 :hr (147 :max 155))
  (:work 5:51.87 1.00 :hr (146 :max 150))
  (:work 5:53.78 1.00 :hr (148 :max 154))
  (:work 5:28.59 1.00 :hr (153 :max 156))
  (:work 5:32.60 1.00 :hr (155 :max 165))
  (:cool 6:12.20 1.00 :hr (148 :max 162))
  (:cool 1:36.98 .241 :hr (144 :max 146 :min 141 :end 142)))

(defworkout "2009-05-22" "09.49"
  (:warm 6:23.36 1.00 :hr (124 :max 132))
  (:warm 6:04.29 1.00 :hr (135 :max 142))
  (:work 5:44.99 1.00 :hr (143 :max 147))
  (:work 5:45.03 1.00 :hr (145 :max 151))
  (:work 5:41.16 1.00 :hr (147 :max 151))
  (:work 5:43.24 1.00 :hr (148 :max 153))
  (:work 5:41.62 1.00 :hr (148 :max 153))
  (:work 5:46.44 1.00 :hr (148 :max 152))
  (:work 6:06.43 1.00 :hr (145 :max 149))
  (:cool 2:09.78 .334 :hr (142 :max 146 :min 138 :end 139)))

(defworkout "2009-05-24" "10.09"
  (:warm 6:34.08 1.00 :hr (128 :max 138))
  (:warm 6:11.50 1.00 :hr (140 :max 146))
  (:work 6:00.45 1.00 :hr (147 :max 152))
  (:work 5:54.16 1.00 :hr (152 :max 156))
  (:work 5:46.46 1.00 :hr (155 :max 158))
  (:work 5:38.73 1.00 :hr (156 :max 159))
  (:work 5:25.13 1.00 :hr (162 :max 167))
  (:work 5:08.86 1.00 :hr (169 :max 174))
  (:work 4:35.35 1.00 :hr (175 :max 177))
  (:cool 5:32.98 1.00 :hr (166 :max 176))
  (:cool 6:01.94 1.00 :hr (157 :max 162))
  (:cool 2:12.45 .338 :hr (151 :max 153)))

(defworkout "2009-05-25" "11.20"
  (:warm 6:21.37 1.00 :hr (132 :max 144))
  (:warm 5:50.99 1.00 :hr (147 :max 156))
  (:work 5:55.64 1.00 :hr (149 :max 152))
  (:work 5:50.44 1.00 :hr (151 :max 158))
  (:work 5:42.96 1.00 :hr (152 :max 155))
  (:work 5:39.66 1.00 :hr (155 :max 160))
  (:work 5:25.37 1.00 :hr (157 :max 160))
  (:work 5:50.03 1.00 :hr (154 :max 159))
  (:work 5:59.85 1.00 :hr (152 :max 157))
  (:work 1:22.32 .195 :hr (149 :max 152)))

(defworkout "2009-05-27" "11.30"
  (:warm 6:46.46 1.00 :hr (133 :max 138))
  (:warm 6:20.12 1.00 :hr (142 :max 147))
  (:work 5:57.54 1.00 :hr (149 :max 154))
  (:work 5:15.71 1.00 :hr (151 :max 157))
  (:work 5:27.82 1.00 :hr (152 :max 157))
  (:work 5:31.10 1.00 :hr (153 :max 156))
  (:cool 1:51.34 .299 :hr (146 :max 155)))

(defworkout "2009-05-29" "11.08"
  (:warm 9:21.2 1500)
  ;pause 13:21.0
  (:warm 7:50.5 1500)
  ;pause 1:11.3
  (:warm 0:48.7  200)
  ;pause 5:34.5
  (:work 0:42.1  200)
  (:rest 3:41.5    0)
  (:work 0:40.7  200)
  (:rest 3:27.2    0)
  (:work 0:39.2  200)
  (:rest 3:27.5    0)
  (:work 0:38.4  200)
  (:rest 3:53.3    0)
  (:work 0:36.1  200)
  ;pause 6:15.3
  (:work 0:10.3   60)
  (:rest 1:47.2    0)
  (:work 0:12.0   60)
  ;pause 3:04.7
  (:cool 7:49.3 1500)
  ;pause 8:09.9
  (:cool 8:38.9 1500))

(defworkout "2009-05-31" "17.00"
  (:warm 6:16.44 1.00 :hr (134 :max 146))
  (:warm 5:59.41 1.00 :hr (147 :max 151))
  (:work 5:59.70 1.00 :hr (149 :max 152))
  (:work 6:03.23 1.00 :hr (152 :max 157))
  (:work 6:49.69 1.00 :hr (158 :max 164))
  (:work 5:46.85 1.00 :hr (155 :max 160))
  (:work 5:38.11 1.00 :hr (154 :max 158))
  (:work 5:57.91 1.00 :hr (155 :max 158))
  (:work 6:07.86 1.00 :hr (154 :max 158))
  (:work 5:56.65 1.00 :hr (156 :max 160))
  (:work 6:14.80 1.00 :hr (156 :max 158))
  (:work 5:47.23 1.00 :hr (157 :max 160))
  (:work 5:37.78 1.00 :hr (160 :max 165))
  (:work 5:38.42 1.00 :hr (166 :max 172))
  (:work 5:47.89 1.00 :hr (162 :max 172))
  (:cool 3:55.57 .646 :hr (161 :max 167)))

(defworkout "2009-06-02" "12.45"
  (:warm 6:30.23 1.00 :hr 127)
  (:warm 6:17.01 1.00 :hr 140)
  (:work 5:58.96 1.00 :hr 148)
  (:work 6:02.89 1.00 :hr 148)
  (:work 5:37.71 1.00 :hr 147)
  (:work 6:21.45 1.00 :hr (155 :max 172))
  (:work 6:12.44 1.00 :hr 151)
  (:work 6:14.53 1.00 :hr 152)
  (:cool 1:44.78 .256 :hr 152))

(defworkout "2009-06-03" "11.03"
  (:warm 6:28.44 1.00 :hr 130)
  (:warm 6:11.50 1.00 :hr 143)
  (:warm 5:27.51 1.00 :hr 157)
  (:work 5:07.41 1.00 :hr 166)
  (:work 5:15.02 1.00 :hr 168)
  (:work 5:05.39 1.00 :hr 168)
  (:work 4:58.78 1.00 :hr 172)
  (:work 4:38.25 1.00 :hr (174 :max 178))
  (:cool 5:44.12 1.00 :hr 166)
  (:cool 5:46.82 .908 :hr 155))

(defworkout "2009-06-05" "10.47"
  (:warm 6:30.35 1.00 :hr 128)
  (:warm 6:06.17 1.00 :hr 137)
  (:work 5:50.57 1.00 :hr 146)
  (:work 5:53.88 1.00 :hr 148)
  (:work 5:57.10 1.00 :hr 150)
  (:work 1:17.22 .221 :hr 155)
  (:hill 0:32.55 .117 :hr 165)
  (:work 1:33.55 .246 :hr 163)
  (:hill 0:30.00 .124 :hr 164)
  (:work 2:01.33 .293 :hr 160)
  (:hill 0:29.75 .116 :hr 165)
  (:work 2:08.60 .306 :hr 162)
  (:hill 0:28.90 .119 :hr (167 :max 180))
  (:cool 6:19.65 1.00 :hr 158)
  (:cool 4:39.00 .689 :hr 151))

(defworkout "2009-06-07" "10.31"
  (:warm 6:36.54 1.00 :hr (126 :max 130))
  (:warm 6:11.22 1.00 :hr (136 :max 145))
  (:work 6:05.76 1.00 :hr (144 :max 149))
  (:work 6:11.45 1.00 :hr (144 :max 148))
  (:work 6:26.54 1.00 :hr (146 :max 151))
  (:work 6:12.71 1.00 :hr (146 :max 154))
  (:work 6:11.00 1.00 :hr (149 :max 153))
  (:work 6:11.83 1.00 :hr (148 :max 154))
  (:work 6:17.53 1.00 :hr (148 :max 154))
  (:work 5:55.81 1.00 :hr (154 :max 164))
  (:work 5:06.42 1.00 :hr (164 :max 172))
  (:work 5:23.53 1.00 :hr (162 :max 172))
  (:work 5:58.27 1.00 :hr (160 :max 175))
  (:cool 5:14.66 .791 :hr (148 :max 152)))

(defworkout "2009-06-08" "15.04"
  (:warm 10:02.3 1600)
  ;pause 28:03.6
  (:warm  6:16.2 1200)
  ;pause  1:35.1
  (:warm  0:20.2  100)
  ;pause  3:07.3
  (:warm  0:21.2  100)
  ;pause  1:46.8
  (:work  1:10.5  300)
  (:rest  0:57.4)
  (:work  1:07.5  300)
  (:rest  1:00.2)
  (:work  1:05.3  300)
  (:rest  6:13.9)
  (:work  1:06.5  300)
  (:rest  0:31.4)
  (:work  1:04.7  300)
  (:rest  0:31.2)
  (:work  1:00.1  300)
  ;pause 18:20.7
  (:cool  7:19.7 1200)) 

(defworkout "2009-06-12" "10.56"
  (:warm 6:43.83 1.00 :hr (127 :max 137))
  (:warm 6:16.04 1.00 :hr (139 :max 145))
  (:warm 5:57.09 1.00 :hr (144 :max 149))
  (:warm 5:44.81 1.00 :hr (151 :max 156))
  (:work 5:28.31 1.00 :hr (157 :max 162))
  (:work 5:35.10 1.00 :hr (160 :max 166))
  (:work 5:16.28 1.00 :hr (167 :max 171))
  (:work 5:14.41 1.00 :hr (171 :max 173))
  (:work 4:43.74 1.00 :hr (176 :max 179))
  (:cool 2:44.91 .444 :hr (164 :max 179 :end 155)))

(defworkout "2009-06-14" "11.04"
  (:warm 6:44.52 1.00)
  (:warm 6:12.75 1.00)
  (:work 5:51.03 1.00)
  (:work 5:46.99 1.00)
  (:work 5:33.50 1.00)
  (:work 5:36.29 1.00)
  (:work 5:35.91 1.00)
  (:work 5:36.82 1.00)
  (:work 2:56.51 .532)
  ;pause 1:38.85
  (:work 0:28.42 .100) ;.102 (103.49m unadjusted)
  ;pause 1:33.10
  (:work 0:20.10 .100) ;.089 (90.36m)
  ;pause 1:45.00
  (:work 0:19.90 .100) ;.102 (103.58m)
  ;pause 1:58.65
  (:work 0:20.23 .100) ;.100 (101.42m)
  (:cool 3:25.37 .533))

(defworkout "2009-06-16" "17.33"
  (:warm 10:54.6 1.815 :hr (148 :max 170))
  ;pause  4:31.3            138/165
  (:warm  0:19.8   100 :hr (154 :max 166))
  ;pause  1:38.2            152/169\140
  (:warm  0:19.8   100 :hr (152 :max 168))
  ;pause  3:43.5            138/171
  (:warm  0:38.2   185 :hr (156 :max 173))
  ;pause  2:06.5            152/176\139
  (:warm  0:42.5   200 :hr (164 :max 176))
  ;pause
  ;18:04
  (:work  0:42.0   200 :hr (167 :max 178))
  (:work  0:43.2   200 :hr (182 :max 185))
  (:work  0:43.8   200 :hr (186 :max 187))
  (:work  0:41.0   200 :hr (188 :max 189))
  (:work  0:36.7   200 :hr (189 :max 190))
  ;pause  0:54.6            182/190\174
  (:cool  5:22.0 0.907 :hr (157 :max 165)))

(defworkout "2009-06-18" "10.06"
  (:warm 6:41.07 1.00 :hr (131 :max 139))
  (:warm 6:20.01 1.00 :hr (142 :max 150))
  (:warm 5:48.35 1.00 :hr (152 :max 159))
  (:work 5:17.47 1.00 :hr (160 :max 167))
  (:work 4:59.81 1.00 :hr (169 :max 172))
  (:work 4:50.35 1.00 :hr (171 :max 175))
  (:work 4:41.95 1.00 :hr (174 :max 177))
  (:work 4:29.24 1.00 :hr (178 :max 180))
  (:cool 5:52.86 1.00 :hr (163 :max 179 :end 152))
  (:cool 1:03.14 .144 :hr (152 :max 154 :end 151)))

(defworkout "2009-06-24" "10.39"
  (:warm 6:40.61 1.00 :hr (131 :max 143))
  (:warm 6:07.51 1.00 :hr (149 :max 154))
  (:work 5:58.70 1.00 :hr (156 :max 162))
  (:work 6:13.90 1.00 :hr (159 :max 162))
  (:work 5:53.42 1.00 :hr (164 :max 166))
  (:work 5:43.02 1.00 :hr (164 :max 169))
  (:work 5:42.25 1.00 :hr (167 :max 170))
  (:work 1:23.53 .239 :hr (167 :max 169)))

(defworkout "2009-06-28" "10.14"
  (:warm 6:50.29 1.00 :hr (131 :max 140))
  (:warm 6:23.69 1.00 :hr (144 :max 149))
  (:work 6:04.77 1.00 :hr (151 :max 156))
  (:work 6:03.67 1.00 :hr (153 :max 157))
  (:work 5:30.24 1.00 :hr (161 :max 167))
  (:work 5:22.28 1.00 :hr (167 :max 172))
  (:work 5:00.96 1.00 :hr (169 :max 174))
  (:work 4:54.61 1.00 :hr (174 :max 177))
  (:work 2:18.16 .506 :hr (180 :max 183))
  (:cool 4:31.43 .727 :hr (168 :max 181 :end 163 :min 161)))

(defworkout "2009-06-29" "9.53"
  (:warm 6:37.40 1.00 :hr (136 :max 144))
  (:warm 6:12.07 1.00 :hr (149 :max 154))
  (:work 5:50.27 1.00 :hr (157 :max 162))
  (:work 5:48.02 1.00 :hr (162 :max 167))
  (:work 1:50.73 .315 :hr (162 :max 166))
  (:hill 0:46.65 .200 :hr (176 :max 182)) ;199m / 1.01 = 197m
  (:work 5:37.90 1.00 :hr (168 :max 181 :end 163))
  (:work 5:49.46 1.00 :hr (165 :max 168))
  (:cool 3:10.46 .508 :hr (162 :max 167 :end 160 :min 158)))

(defworkout "2009-07-01" "9.57"
  (:warm 6:28.29 1.00 :hr (137 :max 144))
  (:warm 6:10.20 1.00 :hr (147 :max 151))
  (:work 5:53.06 1.00 :hr (154 :max 158))
  (:work 5:50.19 1.00 :hr (158 :max 160))
  (:work 5:32.46 1.00 :hr (162 :max 167))
  (:work 5:27.39 1.00 :hr (165 :max 168))
  (:work 5:05.19 1.00 :hr (170 :max 176))
  (:cool 1:13.01 .209 :hr (171 :max 176 :end 168)))

(defworkout "2009-07-03" "10.07"
  (:warm 6:39.08 1.00)
  (:warm 6:13.14 1.00)
  (:work 5:57.54 1.00)
  (:work 5:49.53 1.00)
  (:work 5:45.57 1.00)
  (:work 5:37.41 1.00)
  (:work 1:20.57 .250)
  (:rest 1:29.32 0)
  (:work 0:22.30 100) ;103
  (:rest 1:40.93 0)
  (:work 0:20.73 100) ;97
  (:rest 2:03.42 0)
  (:work 0:18.92 100) ;88
  (:work 3:56.41 .699))

(defworkout "2009-07-05" "10.28"
  (:warm 6:20.93 1.00)
  (:warm 6:11.77 1.00)
  (:warm 5:53.19 1.00)
  (:warm 5:35.10 1.00)
  (:work 5:01.89 1.00)
  (:work 4:56.38 1.00)
  (:work 4:50.17 1.00)
  (:work 4:50.32 1.00)
  (:work 4:38.91 1.00)
  (:work 4:33.52 1.00)
  (:cool 5:54.04 1.00)
  (:cool 1:39.27 .272))

(defworkout "2009-07-06" "10.09"
  (:warm 6:46.34 1.00 :hr (131 :max 138))
  (:warm 6:14.14 1.00 :hr (143 :max 147))
  (:work 5:55.82 1.00 :hr (150 :max 153))
  (:work 5:52.33 1.00 :hr (155 :max 159))
  (:work 2:33.51 .445 :hr (158 :max 164))
  (:hill 0:51.52  200 :hr (167 :max 173))
  (:rest 2:44.40    0 :hr (139 :max 173 :end 129 :min 124))
  (:hill 0:40.28  200 :hr (159 :max 174))
  (:rest 1:13.65    0 :hr (159 :max 175 :end 139))
  (:work 5:46.20 1.00 :hr (151 :max 155))
  (:work 5:53.13 1.00 :hr (154 :max 158))
  (:cool 1:42.95 .264 :hr (152 :max 154 :end 149)))

(defworkout "2009-07-08" "10.18"
  (:warm 6:38.96 1.00 :hr (135 :max 143))
  (:warm 6:08.61 1.00 :hr (144 :max 147))
  (:work 6:01.64 1.00 :hr (152 :max 157))
  (:work 5:40.10 1.00 :hr (161 :max 166))
  (:work 5:13.65 1.00 :hr (167 :max 170))
  (:work 5:02.06 1.00 :hr (170 :max 176))
  (:work 4:55.47 1.00 :hr (173 :max 176))
  (:work 4:38.42 1.00 :hr (176 :max 178))
  (:cool 1:25.76 .235 :hr (170 :max 177 :end 164)))

(defworkout "2009-07-10" "10.17"
  (:warm 6:41.03 1.00 :hr (133 :max 133))
  (:warm 6:09.47 1.00 :hr (139 :max 139))
  (:work 5:58.06 1.00 :hr (146 :max 146))
  (:work 5:45.22 1.00 :hr (151 :max 151))
  (:work 5:06.40 1.00 :hr (162 :max 170))
  (:work 5:33.76 1.00 :hr (159 :max 159))
  (:work 1:42.97 .278 :hr (157 :max 157))
  (:rest 0:24.60    0 :hr (135 :max 150))
  (:work 0:21.92  100 :hr (147 :max 160)) ;101
  (:rest 1:40.32    0 :hr (140 :max 160))
  (:work 0:20.65  100 :hr (145 :max 160)) ;108
  (:rest 1:37.95    0 :hr (143 :max 160))
  (:work 0:18.77  100 :hr (143 :max 160)) ;96
  (:rest 2:17.31    0 :hr (140 :max 160))
  (:work 0:16.92  100 :hr (142 :max 160)) ;101
  (:rest 0:50.67    0 :hr (162 :max 170))
  (:work 3:53.47 .638 :hr (150 :max 150)))

(defworkout "2009-07-12" "10.38"
  (:warm 6:49.59 1.00 :hr (129 :max 143))
  (:warm 6:27.65 1.00 :hr (141 :max 146))
  (:work 6:07.63 1.00 :hr (146 :max 148))
  (:work 6:09.66 1.00 :hr (150 :max 158))
  (:work 6:51.03 1.00 :hr (157 :max 167))
  (:work 5:46.01 1.00 :hr (148 :max 159))
  (:work 6:01.48 1.00 :hr (149 :max 154))
  (:work 5:54.08 1.00 :hr (154 :max 157))
  (:work 1:20.46 .218 :hr (152 :max 154))
  (:rest 0:58.26    0 :hr (145 :max 152 :min 130))
  (:work 1:02.40 .183 :hr (139 :max 147))
  (:rest 0:51.49    0 :hr (149 :max 155 :min 144))
  (:work 1:21.48 .238 :hr (145 :max 150))
  (:rest 3:47.33    0 :hr (132 :max 150 :min 112))
  (:work 5:52.12 1.00 :hr (147 :max 154))
  (:work 5:35.65 1.00 :hr (156 :max 161))
  (:work 5:36.80 1.00 :hr (162 :max 168))
  (:work 5:25.84 1.00 :hr (165 :max 172))
  (:cool 4:35.29 .768 :hr (157 :max 161 :min 154)))

(defworkout "2009-07-13" "10.11"
  (:warm 6:35.97 1.00 :hr (127 :max 132))
  (:warm 6:11.86 1.00 :hr (138 :max 141))
  (:work 5:56.72 1.00 :hr (144 :max 150))
  (:work 5:48.65 1.00 :hr (150 :max 154))
  (:work 2:32.46 .464 :hr (151 :max 153))
  (:hill 0:51.69  200 :hr (166 :max 172))
  (:rest 2:38.96    0 :hr (137 :max 172 :min 118))
  (:hill 0:45.10  200 :hr (156 :max 171))
  (:rest 0:42.38    0 :hr (164 :max 171 :min 151))
  (:work 5:50.02 1.00 :hr (149 :max 152))
  (:work 5:45.08 1.00 :hr (153 :max 156))
  (:cool 1:53.50 .317 :hr (150 :max 153 :min 147)))

(defworkout "2009-07-15" "11.01"
  (:warm 6:42.73 1.00)
  (:warm 6:17.40 1.00)
  (:work 5:51.41 1.00)
  (:work 5:31.72 1.00)
  (:work 5:00.76 1.00)
  (:cool 1:03.16 .169))

(defworkout "2009-07-16" "15.24"
  (:warm 6:37.67 1.00 :hr (137 :max 147))
  (:warm 6:08.83 1.00 :hr (150 :max 159))
  (:warm 5:29.86 1.00 :hr (162 :max 171))
  (:work 4:49.02 1.00 :hr (172 :max 174))
  (:work 4:46.16 1.00 :hr (173 :max 175))
  (:work 4:42.60 1.00 :hr (176 :max 179))
  (:work 4:31.34 1.00 :hr (178 :max 180))
  (:cool 5:54.86 1.00 :hr (163 :max 179))
  (:cool 1:02.53 .161 :hr (157 :max 158)))

(defworkout "2009-07-17" "10.13"
  (:warm 6:36.20 1.00)
  (:warm 6:07.74 1.00)
  (:work 5:51.75 1.00)
  ;(:work 0:06.90 .016)
  ;pause 4:08.08
  (:work 5:20.65 1.00)
  (:work 0:29.57 .082)
  (:rest 1:07.57    0)
  (:work 0:37.03  150) ;151
  (:rest 2:03.85    0)
  (:work 0:33.28  150) ;153
  (:rest 1:58.84    0)
  (:work 0:30.43  150) ;152
  (:cool 3:17.33 .545))

(defworkout "2009-07-19" "11.13"
  (:warm 6:41.07 1.00)
  (:warm 6:12.77 1.00)
  (:work 5:56.48 1.00)
  (:work 4:06.82 .687)
  (:work 6:38.35 1.00)
  (:work 5:47.08 1.00)
  (:work 5:40.91 1.00)
  (:work 5:47.65 1.00)
  (:work 5:47.53 1.00)
  (:work 5:41.32 1.00)
  (:work 5:46.93 1.00)
  (:work 5:29.25 1.00)
  (:work 5:32.13 1.00)
  (:work 5:40.50 1.00)
  (:work 5:17.48 1.00)
  (:cool 2:38.20 .503))

(defworkout "2009-07-20" "10.10"
  (:warm 6:28.39 1.00)
  (:warm 6:04.22 1.00)
  (:work 5:52.68 1.00)
  (:work 5:41.56 1.00)
  (:work 2:49.27 .511)
  (:hill 0:38.32  150)
  (:rest 1:50.10    0)
  (:hill 0:34.58  150)
  (:rest 1:56.12    0)
  (:hill 0:30.74  150)
  (:rest 1:56.99    0)
  (:hill 0:29.65  150)
  (:rest 0:30.57    0)
  (:work 5:39.62 1.00)
  (:work 5:28.53 1.00)
  (:cool 1:57.45 .328))

(defworkout "2009-07-22" "10.35"
  (:warm 6:43.82 1.00 :hr (131 :max 140))
  (:warm 6:06.95 1.00 :hr (144 :max 148))
  (:warm 5:34.10 1.00 :hr (158 :max 168))
  (:work 4:51.08 1.00 :hr (170 :max 171))
  (:work 4:48.10 1.00 :hr (171 :max 175))
  (:work 4:42.14 1.00 :hr (173 :max 178))
  (:work 4:45.53 1.00 :hr (176 :max 178))
  (:work 4:35.93 1.00 :hr (177 :max 179))
  (:cool 5:28.02 1.00 :hr (166 :max 176 :min 162))
  (:cool 6:05.80 1.00 :hr (156 :max 166 :min 150))
  (:cool 0:29.52 .073 :hr (151 :max 152)))

(defworkout "2009-07-24" "10.08"
  (:warm 6:36.62 1.00)
  (:warm 6:07.85 1.00)
  (:work 5:46.82 1.00)
  (:work 5:42.25 1.00)
  (:work 5:33.11 1.00)
  (:work 5:37.02 1.00)
  (:work 3:32.52 .667) ;
  (:rest 2:53.52    0)
  (:work 0:21.78  100) ;103
  (:rest 1:36.82    0)
  (:work 0:19.33  100) ;101
  (:rest 1:34.55    0)
  (:work 0:19.45  100) ;100
  (:rest 1:45.95    0)
  (:work 0:17.68  100) ;104
  (:rest 2:04.24    0)
  (:work 0:15.76  100) ;101
  (:rest 1:06.15    0)
  (:cool 6:00     1.0));?

(defworkout "2009-07-26" "11.47"
  (:warm 6:31.22 1.00)
  (:warm 6:00.21 1.00)
  (:work 5:43.38 1.00)
  (:work 5:45.44 1.00)
  (:work 5:46.59 1.00)
  (:work 5:39.88 1.00)
  (:work 5:43.17 1.00)
  (:work 5:41.00 1.00)
  (:work 5:52.72 1.00)
  (:work 5:48.49 1.00)
  (:work 5:32.37 1.00)
  (:work 5:31.72 1.00)
  (:work 5:28.83 1.00)
  (:work 5:12.18 1.00)
  ;pause 1:34.39
  (:cool 6:01.17 1.00))

(defworkout "2009-07-27" "10.18"
  (:warm 6:17.54 1.00 :hr (128 :max 135))
  (:warm 5:59.42 1.00 :hr (138 :max 142))
  (:work 5:54.50 1.00 :hr (145 :max 149))
  (:work 5:53.76 1.00 :hr (147 :max 152))
  (:work 5:51.84 1.00 :hr (147 :max 150))
  (:work 5:38.46 1.00 :hr (147 :max 150))
  (:work 3:28.50 .596 :hr (150 :max 156))
  (:rest 0:45.88    0 :hr (145 :max 156))
  (:rest 0:22.87    0 :hr (129 :max 130)) ;36+
  (:rest 0:22.62    0 :hr (130 :max 131)) ;27=63
  (:rest 0:46.32    0 :hr (131 :max 134))
  (:rest 0:40.91    0 :hr (122 :max 129)) ;61
  (:rest 0:41.36    0 :hr (126 :max 137))
  (:hill 0:13.82   62 :hr (146 :max 155)) ;69
  (:rest 1:06.52    0 :hr (142 :max 161 :min 127))
  (:hill 0:12.48   62 :hr (143 :max 156)) ;66
  (:rest 1:19.90    0 :hr (142 :max 164 :min 122))
  (:hill 0:11.20   62 :hr (143 :max 153)) ;62
  (:rest 1:06.37    0 :hr (151 :max 168 :min 130))
  (:work 6:01.10 1.00 :hr (144 :max 148 :min 142)))

(defworkout "2009-07-29" "10.31"
  (:warm 6:42.67 1.00 :hr (129 :max 138))
  (:warm 6:11.73 1.00 :hr (139 :max 142))
  (:warm 5:51.94 1.00 :hr (148 :max 153))
  (:warm 5:18.68 1.00 :hr (161 :max 167))
  (:work 4:40.29 1.00 :hr (169 :max 173))
  (:work 4:36.63 1.00 :hr (173 :max 176))
  (:work 4:36.76 1.00 :hr (174 :max 176))
  (:work 4:27.23 1.00 :hr (176 :max 178))
  (:cool 5:37.22 1.00 :hr (161 :max 174 :min 156))
  (:cool 6:06.79 1.00 :hr (150 :max 150 :min 145))
  (:cool 0:24.75 .057 :hr (144 :max 146 :min 143)))

(defworkout "2009-07-31" "10.26"
  (:warm 6:40.93 1.00)
  (:warm 6:16.38 1.00)
  (:work 5:51.48 1.00)
  (:work 5:41.31 1.00)
  (:work 5:34.69 1.00)
  (:work 4:14.95 .815)
  (:rest 1:29.30    0)
  (:work 0:20.40  100) ;105.8
  (:rest 1:26.32    0)
  (:work 0:19.90  100) ;104.8
  (:rest 2:23.68    0)
  (:work 0:48.26  200) ;202.8
  (:rest 0:29.69    0)
  (:work 0:46.20  200) ;201.7
  (:rest 0:30.70    0)
  (:work 0:46.20  200) ;207.3
  (:rest 0:30.07    0)
  (:work 0:44.33  200) ;202.4
  (:rest 0:29.72    0)
  (:work 0:45.38  200) ;201.8
  (:rest 0:28.92    0)
  (:work 0:44.68  200) ;202.8
  (:rest 0:29.82    0)
  (:work 0:44.65  200) ;198.8
  (:rest 0:31.43    0)
  (:work 0:38.97  200) ;202.3
  (:rest 1:22.50    0)
  (:cool 4:08.53 .755))

(defworkout "2009-08-02" "11.11"
  (:warm 6:21.00 1.00)
  (:warm 5:54.86 1.00)
  (:work 5:51.05 1.00)
  (:work 5:51.84 1.00)
  (:work 6:08.71 1.00)
  (:work 5:30.23 1.00)
  (:work 5:31.88 1.00)
  (:work 5:41.45 1.00)
  (:work 5:49.48 1.00)
  (:work 5:45.52 1.00)
  (:work 5:49.37 1.00)
  (:work 5:46.93 1.00)
  (:work 5:33.21 1.00)
  (:work 5:30.13 1.00)
  (:work 2:06.43 .341)
  (:rest 0:52.17    0)
  (:work 5:01.60 1.00)
  (:cool 4:28.43 .753))

(defworkout "2009-08-03" "10.16"
  (:warm 6:43.21 1.00)
  (:warm 5:50.11 1.00)
  (:work 5:40.61 1.00)
  (:work 5:32.46 1.00)
  (:work 5:33.31 1.05)
  (:work 4:55.25 0.95)
  (:work 3:43.42  .65)
  (:rest 1:57.77    0)
  (:hill 0:07.89   40)
  (:rest 1:02.81    0)
  (:hill 0:07.50   40)
  (:rest 1:01.40    0)
  (:hill 0:06.76   40)
  (:rest 1:26.20    0)
  (:work 6:06.04 1.00))

(defworkout "2009-08-05" "10.16"
  (:warm 6:38.02 1.00)
  (:warm 6:13.54 1.00)
  (:warm 5:35.32 1.00)
  (:work 4:49.72 1.00)
  (:work 4:42.90 1.00)
  (:work 4:46.74 1.00)
  (:work 4:46.47 1.00)
  (:work 4:49.03 1.00)
  (:work 4:38.53 1.00)
  (:cool 5:30.04 1.00)
  (:cool 5:58.03 1.00)
  (:rest 1:22.37    0)
  (:work 0:20.60  100)
  (:rest 1:40.75    0)
  (:work 0:17.05  100)
  (:rest 2:11.25    0)
  (:work 0:16.39  100)
  (:rest 1:07.06    0)
  (:cool 3:56.96 .641))

(defworkout "2009-08-09" "12.08"
  (:warm 6:24.33 1.00)
  (:warm 6:13.34 1.00)
  (:work 5:54.46 1.00)
  (:work 5:56.23 1.00)
  (:work 6:24.84 1.00)
  (:work 5:43.16 1.00)
  (:work 5:34.34 1.00)
  (:work 5:49.83 1.00)
  (:work 1:46.80 .317)
  (:rest 5:45.38    0)
  (:work 5:50.68 1.00)
  (:work 5:42.97 1.00)
  (:work 5:52.24 1.00)
  (:work 5:37.92 1.00)
  (:work 5:30.67 1.00)
  (:work 5:29.71 1.00)
  (:work 5:50.27 1.00)
  (:work 5:19.90 1.00)
  (:cool 3:47.49 .625))

(defworkout "2009-08-11" "15.24"
  (:warm 6:33.51 1.00)
  (:warm 5:14.30 1.00)
  (:work 5:47.84 1.00)
  (:work 2:25.47 .419)
  (:rest 0:17.47    0) ;54.03
  (:rest 1:16.40    0)
  (:hill 0:13.23   60) ;73.20
  (:rest 1:12.85    0)
  (:hill 0:12.17   60) ;74.28
  (:rest 1:15.25    0)
  (:hill 0:11.35   60) ;71.54
  (:rest 1:26.30    0)
  (:hill 0:09.98   60) ;74.63
  (:rest 2:09.52    0)
  (:work 5:36.78 1.00)
  (:work 5:00.45 1.00)
  (:cool 5:55.48 1.00))

(defworkout "2009-08-12" "18.34"
  (:warm 10:15.7 1600 :hr (136 :max 148))
  ;pause  4:31.0    0 :hr (126 :max 147 :end 121)
  (:warm  7:56.2 1600 :hr (162 :max 174))
  ;19:02
  (:warm  0:47.7  200 :hr (156 :max 169))
  ;rest   1:12.0
  (:warm  0:44.6  200 :hr (160 :max 174))
  ;rest   1:38.4
  (:warm  0:42.7  200 :hr (159 :max 174))
  ;rest   2:00 161\128
  ;19:13
  (:work  1:08.0  300 :hr (154 :max 180))
  (:work  1:29.1  400 :hr (182 :max 185))
  (:work  1:28.7  400 :hr (185 :max 188))
  (:work  1:25.6  400 :hr (188 :max 190))
  ;rest   1:00.3 171/190\148
  )

(defworkout "2009-08-14" "10.36"
  (:warm 6:41.66 1.00)
  (:warm 6:10.11 1.00)
  (:warm 5:43.65 1.00)
  (:warm 5:04.56 1.00)
  (:work 4:40.00 1.00)
  (:work 4:39.15 1.00)
  (:work 4:37.59 1.00)
  (:work 4:34.23 1.00)
  (:cool 5:31.71 1.00)
  (:cool 5:44.95 1.00)
  (:cool 1:27.51 .220))

(defworkout "2009-08-16" "16.07"
  (:warm 6:23.80 1.00)
  (:work 5:56.88 1.00)
  (:work 5:56.35 1.00)
  (:work 5:45.16 1.00)
  (:work 5:52.04 1.00)
  (:work 5:51.70 1.00)
  (:work 5:50.88 1.00)
  (:work 5:37.15 1.00)
  (:work 5:41.78 1.00)
  (:work 5:41.51 1.00)
  (:work 6:00.80 1.00)
  (:work 5:08.45 1.00)
  (:work 4:49.45 1.00)
  (:work 4:45.96 1.00)
  (:work 4:17.96 1.00)
  (:cool 5:33.53 1.00)
  (:cool 5:42.45 1.00)
  (:cool 0:42.85 .122))

(defworkout "2009-08-17" "17.03"
  (:warm 6:25.89 1.00)
  (:work 6:01.92 1.00)
  (:work 5:50.92 1.00)
  (:work 5:40.29 1.00)
  (:work 5:58.47 1.00)
  (:work 6:14.24 1.00)
  (:work 5:40.30 1.00)
  (:work 0:52.36 .153)
  (:rest 0:22.07    0)
  (:hill 0:05.55   30) ;29.53
  (:rest 1:01.48    0)
  (:hill 0:05.02   30) ;28.03
  (:rest 1:00.16    0)
  (:hill 0:04.77   30) ;30.17
  (:rest 1:03.75    0)
  (:hill 0:04.52   30) ;28.16
  (:rest 1:08.98    0)
  (:hill 0:04.50   30) ;28.00
  (:rest 0:39.20    0)
  (:work 3:21.22 .579))

(defworkout "2009-08-19" "10.07"
  (:warm 7:01.70 1.00 :hr 129)
  (:warm 6:17.21 1.00 :hr 140)
  (:warm 5:49.33 1.00 :hr 147)
  (:warm 4:38.87 .913 :hr 161)
  ;pause 1:05.70    0     149
  (:work 4:37.75  971 :hr 166) ;1010m
  (:work 0:08.81   29 :hr 170) ;  29m
  (:work 4:33.32  968 :hr 172) ;1010m
  (:work 0:08.80   32 :hr 175) ;  32m
  (:work 4:29.69  979 :hr 176) ;1010m
  (:work 0:06.46   21 :hr 177) ;  21m
  (:work 4:20.48  969 :hr 176) ;1010m
  (:work 0:07.40   31 :hr 178) ;  31m
  (:cool 5:45.70 1.00 :hr 162)
  (:cool 6:23.37 1.00 :hr 155)
  (:cool 0:45.08 .085 :hr 138))

(defworkout "2009-08-21" "11.19"
  (:warm 6:33.32 1.00)
  (:warm 6:07.34 1.00)
  (:work 5:56.90 1.00)
  (:work 5:33.96 1.00)
  (:work 5:35.66 1.00)
  (:work 5:20.12 1.00)
  (:work 1:46.86 .320)
  (:rest 2:43.53    0)
  (:work 0:21.72  100)
  (:rest 1:34.78    0)
  (:work 1:36.97  400)
  (:rest 0:58.13    0)
  (:work 1:36.80  400)
  (:rest 0:58.60    0)
  (:work 1:36.52  400)
  (:rest 0:59.60    0)
  (:work 1:34.80  400)
  (:rest 0:59.78    0)
  (:work 1:33.00  400)
  (:rest 1:00.45    0)
  (:work 1:31.96  400)
  (:rest 1:00.70    0)
  (:cool 2:07.46 .382))

(defworkout "2009-08-30" "20.37"
  (:warm 6:54.61 1.00)
  ;pause 2:00.62
  (:work 2:30.86  400)
  ;pause 2:47.18
  (:work 2:41.15 .434))

(defworkout "2009-08-31" "16.49"
  (:warm 6:42.61 1.00 :hr (124 :max 133))
  (:warm 5:56.98 1.00 :hr (132 :max 138))
  (:work 5:30.71 1.00 :hr (143 :max 150))
  (:work 5:36.84 1.00 :hr (156 :max 165))
  (:work 1:17.02 .225 :hr (143 :max 151))
  (:rest 4:47.70    0 :hr (126 :max 144))	;49.80, 52.69
  (:hill 0:10.32   50 :hr (135 :max 140))	;59.83
  (:rest 1:08.67    0 :hr (139 :max 154 :min 121))
  (:hill 0:10.05   50 :hr (132 :max 142))	;53.43
  (:rest 1:06.13    0 :hr (144 :max 159 :min 125))
  (:hill 0:09.35   50 :hr (137 :max 148))	;53.65
  (:rest 1:12.61    0 :hr (146 :max 164 :min 123))
  (:hill 0:08.61   50 :hr (136 :max 144))	;47.89
  (:rest 1:24.60    0 :hr (157 :max 165 :min 141))
  (:work 5:37.69 1.00 :hr (149 :max 160 :end 145)))

(defworkout "2009-09-02" "16.33"
  (:warm 6:55.44 1.00 :hr (137 :max 153))
  (:warm 6:36.20 1.00 :hr (155 :max 169))
  (:warm 5:16.03 1.00 :hr (165 :max 173))
  (:work 4:39.76 1.00 :hr (173 :max 178))
  (:work 4:28.79 1.00 :hr (177 :max 181))
  (:work 4:21.94 1.00 :hr (175 :max 181))
  (:cool 3:24.22 .655 :hr (170 :max 180 :min 162)))

(defworkout "2009-09-04" "10.08"
  (:warm 6:30.50 1.00 :hr (132 :max 143))
  (:warm 6:11.12 1.00 :hr (144 :max 150))
  (:warm 5:01.79 1.00 :hr (160 :max 178))
  (:rest 1:57.22    0 :hr (145 :max 172 :end 135))
  (:work 1:40.74  400 :hr (167 :max 181)) ;409
  (:rest 0:59.35    0 :hr (168 :max 182 :end 149))
  (:work 1:34.72  400 :hr (172 :max 184)) ;424
  (:rest 0:58.53    0 :hr (172 :max 184 :end 154))
  (:work 1:33.55  400 :hr (174 :max 185)) ;410
  (:rest 0:59.15    0 :hr (174 :max 185 :end 155))
  (:work 1:32.74  400 :hr (176 :max 186)) ;423
  (:rest 1:00.91    0 :hr (176 :max 186 :end 158))
  (:work 1:31.15  400 :hr (177 :max 187)) ;412
  (:rest 0:58.43    0 :hr (178 :max 187 :end 161))
  (:work 1:29.99  400 :hr (178 :max 188)) ;422
  (:rest 1:02.08    0 :hr (177 :max 188 :end 161))
  (:work 1:28.70  400 :hr (180 :max 188)) ;410
  (:rest 1:01.55    0 :hr (180 :max 189 :end 166))
  (:work 1:27.22  400 :hr (181 :max 190)) ;423
  (:rest 1:02.30    0 :hr (179 :max 190 :end 164))
  (:rest 1:16.63    0 :hr (147 :max 162 :end 140))
  (:cool 6:00.81 1.00 :hr (163 :max 172))
  (:cool 4:15.19 .672 :hr (161 :max 168 :end 145)))

(defworkout "2009-09-06" "15.19"
  (:warm 6:38.61 1.00) ;Free 5.0
  ;pause 3:31.55
  (:warm 6:32.77 1.00)
  (:work 5:43.39 1.00)
  (:work 5:43.26 1.00)
  (:work 5:22.23 1.00)
  (:work 5:19.71 1.00)
  (:work 5:09.25 1.00)
  (:work 5:56.00 1.00)
  (:work 5:47.85 1.00)
  (:work 5:44.98 1.00)
  (:work 5:30.48 1.00)
  (:work 4:51.52 1.00)
  (:work 4:47.24 1.00)
  (:work 4:39.57 1.00)
  (:work 1:18.78 .285)
  ;pause 8:22.00
  (:work 1:29.42 .267)
  (:work 2:12.80  400) ;411.2
  (:work 2:09.39  400) ;405.3
  (:cool 2:20.49  400) ;424.0
  (:cool 2:13.32  390)
  (:cool 2:23.08  410)
  (:cool 1:56.83 .321))

(defworkout "2009-09-07" "16.31"
  (:warm 6:36.37 1.00)
  (:warm 6:33.09 1.00)
  (:work 5:42.18 1.00)
  (:work 5:42.20 1.00)
  (:work 5:00.16 1.00)
  (:work 0:57.29 .175)
  (:work 0:22.65 .046) ;46.95
  ;pause 0:43.23
  ;pause 0:30.95       ;51.41
  ;pause 1:37.20
  (:hill 0:12.42   55) ;57.20
  (:rest 0:12.60    0)
  (:hill 0:13.08   55) ;55.79
  (:rest 1:09.27    0)
  (:hill 0:12.45   55) ;57.10
  (:rest 1:20.00    0)
  (:hill 0:10.93   55) ;54.52
  (:rest 1:26.30    0)
  (:hill 0:10.55   55) ;53.28
  (:rest 0:53.47    0)
  (:cool 2:08.36 .384))

(defworkout "2009-09-09" "16.03"
  (:warm 1:57.74 .299 :hr (143 :max 150))
  (:warm 1:17.60  200 :hr (148 :max 150)) ;201.3
  (:rest 0:14.38    0 :hr (147 :max 148))
  (:warm 2:33.02  400 :hr (142 :max 148)) ;403.0
  (:rest 2:48.22    0 :hr (121 :max 149))
  (:warm 2:25.50  400 :hr (142 :max 149)) ;413.3
  (:warm 2:26.73  400 :hr (149 :max 153)) ;403.0
  (:warm 2:17.01  400 :hr (155 :max 158)) ;414.4
  (:warm 2:05.10  400 :hr (164 :max 170)) ;414.3
  (:warm 1:52.24  400 :hr (173 :max 177)) ;419.5
  (:rest 3:57.77    0 :hr (141 :max 173))
  (:work 2:28.83  600 :hr (172 :max 183)) ;617.2
  (:rest 1:30.18  200 :hr (175 :max 183))
  (:work 2:24.42  600 :hr (179 :max 185)) ;617.9
  (:rest 1:31.20  200 :hr (176 :max 185))
  (:work 2:24.60  600 :hr (179 :max 186)) ;617.0
  (:rest 1:31.03  200 :hr (177 :max 184))
  (:work 2:23.80  600 :hr (180 :max 186)) ;630.7
  (:rest 1:33.12  200 :hr (177 :max 185))
  (:work 2:23.33  600 :hr (180 :max 187)) ;633.2
  (:rest 1:30.82  200 :hr (181 :max 187))
  (:work 2:21.28  600 :hr (181 :max 189)) ;617.0
  (:rest 1:14.46  200 :hr (183 :max 189))
  (:rest 0:16.19    0 :hr (176 :max 178))
  (:rest 0:32.65    0 :hr (164 :max 173))
  (:cool 2:26.22  400 :hr (159 :max 164)) ;402.7
  (:rest 3:40.20    0 :hr (142 :max 164))
  (:work 0:17.94  100 :hr (157 :max 165)) ;109.2
  (:rest 3:30.46    0 :hr (146 :max 172))
  (:work 0:15.80  100 :hr (153 :max 165)) ;102.9
  (:rest 6:17.68    0 :hr (139 :max 172))
  (:cool 2:45.22 .426 :hr (151 :max 158)))

(defworkout "2009-09-11" "16.27"
  (:warm 6:37.15 1.00)
  (:warm 6:09.02 1.00)
  (:warm 5:43.93 1.00)
  (:warm 5:14.98 1.00)
  ;pause 6:27.01
  (:work 4:38.09 1.00)
  (:work 4:34.10 1.00)
  (:work 4:30.95 1.00)
  (:work 4:23.18 1.00)
  (:cool 5:22.95 1.00))

(defworkout "2009-09-13" "18.33"
  (:warm 6:34.78 1.00)
  (:warm 6:08.54 1.00)
  (:warm 6:18.61 1.00)
  (:warm 5:59.24 1.00)
  (:warm 5:37.36 1.00)
  (:warm 5:25.11 1.00)
  (:warm 5:35.72 1.00)
  (:warm 5:39.78 1.00)
  (:warm 5:24.94 1.00)
  (:work 4:57.07 1.00)
  (:work 5:20.45 1.00)
  (:work 4:56.10 1.00)
  (:rest 6:01.63 1.00)
  (:work 5:01.70 1.00)
  (:work 4:43.85 1.00)
  (:work 4:16.24 1.00)
  (:cool 5:23.51 1.00)
  (:cool 5:46.99 1.00)
  (:cool 2:34.62 .438))

(defworkout "2009-09-14" "16.39"
  (:warm 6:25.32 1.00 :hr (127 :max 142))
  (:warm 6:17.08 1.00 :hr (139 :max 154))
  (:work 6:19.02 1.00 :hr (150 :max 158))
  (:work 6:04.79 1.00 :hr (154 :max 159))
  (:work 5:05.02 .954 :hr (151 :max 168))
  (:rest 2:46.33    0 :hr (142 :max 165))
  (:hill 0:10.98   45 :hr (146 :max 155)) ;43.9
  (:rest 1:29.27    0 :hr (147 :max 166))
  (:hill 0:10.79   45 :hr (145 :max 153)) ;49.2
  (:rest 1:35.26    0 :hr (147 :max 165))
  (:hill 0:09.99   45 :hr (145 :max 151)) ;45.3
  (:rest 1:32.01    0 :hr (149 :max 165))
  (:hill 0:09.65   45 :hr (141 :max 147)) ;47.5
  (:rest 1:29.05    0 :hr (151 :max 166))
  (:hill 0:09.30   45 :hr (146 :max 152)) ;42.6
  (:rest 1:54.03    0 :hr (151 :max 170))
  (:hill 0:08.67   45 :hr (148 :max 155)) ;49.6
  (:rest 1:00.38    0 :hr (163 :max 170))
  (:cool 2:17.43 .395 :hr (155 :max 159)))

(defworkout "2009-09-16" "15.58"
  (:warm 3:35.07 .536 :hr (140 :max 149))
  (:warm 2:41.55  400 :hr (145 :max 150)) ;407.5
  (:warm 2:31.77  400 :hr (147 :max 152)) ;397.7
  (:warm 2:22.30  400 :hr (153 :max 155)) ;402.5
  ;pause 2:01.63    0 :hr (128 :max 153)
  (:warm 2:18.97  400 :hr (144 :max 151)) ;411.8
  (:warm 2:20.82  400 :hr (150 :max 153)) ;404.3
  (:warm 2:18.56  400 :hr (154 :max 159)) ;407.2
  (:warm 1:52.70  400 :hr (170 :max 178)) ;413.0
  ;pause 1:31.67    0 :hr (151 :max 178)
  (:warm 0:17.73  100 :hr (153 :max 170)) ;110.8
  ;pause 2:16.57    0 :hr (149 :max 176)
  (:work 3:27.13  800 :hr (173 :max 182)) ;823.8
  (:rest 2:00.65  200 :hr (166 :max 182))
  (:work 3:26.36  800 :hr (175 :max 183)) ;809.4
  (:rest 2:00.18  200 :hr (169 :max 181))
  (:work 3:19.28  800 :hr (176 :max 185)) ;803.7
  (:rest 2:00.93  200 :hr (170 :max 184))
  (:work 3:16.77  800 :hr (178 :max 185)) ;819.6
  (:rest 1:59.20  200 :hr (172 :max 183))
  (:work 2:55.25  700 :hr (179 :max 185)) ;705.4
  (:work 0:16.57  100 :hr (187 :max 189)));105.1

(defworkout "2009-09-18" "10.09"
  (:warm 6:40.70 1.00 :hr (127 :max 134))
  (:warm 5:13.18 .842 :hr (142 :max 147))
  (:warm 6:30.59 1200 :hr (167 :max 171))
  (:work 4:34.28  970 :hr (174 :max 178))
  (:work 0:08.71   30 :hr (175 :max 176))
  (:work 4:37.45  970 :hr (175 :max 177))
  (:work 0:07.75   30 :hr (174 :max 175))
  (:work 4:35.91  970 :hr (175 :max 177))
  (:work 0:08.04   30 :hr (175 :max 176))
  (:work 4:33.67  970 :hr (176 :max 179))
  (:work 0:10.51   30 :hr (175 :max 176))
  (:work 4:33.09  970 :hr (177 :max 179))
  (:work 0:10.93   30 :hr (178 :max 179))
  (:work 4:27.76  970 :hr (180 :max 183))
  (:work 0:06.00   30 :hr (183 :max 183))
  ;pause 2:01.22    0 :hr (157 :max 182 :end 138)
  (:cool 6:14.93 1.00 :hr (158 :max 164))
  (:cool 1:43.97 .278 :hr (160 :max 162)))

(defworkout "2009-09-20" "11.47"
  (:warm 6:47.87 1.00)
  (:warm 6:07.04 1.00)
  (:warm 5:48.21 1.00)
  (:warm 5:49.54 1.00)
  (:warm 6:33.38 1.00)
  (:warm 5:36.04 1.00)
  (:warm 5:21.92 1.00)
  (:work 5:04.23 1.00)
  (:work 4:59.53 1.00)
  (:work 4:55.36 1.00)
  (:rest 5:36.86 1.00)
  (:work 4:57.48 1.00)
  (:work 4:53.63 1.00)
  (:work 4:56.64 1.00)
  (:rest 5:24.12 1.00)
  (:work 4:53.39 1.00)
  (:work 4:58.16 1.00)
  (:work 4:45.71 1.00)
  (:cool 5:35.97 1.00)
  (:cool 5:53.79 1.00))

(defworkout "2009-09-23" "16.32"
  (:warm 3:32.29 .519 :hr (135 :max 142))
  (:warm 3:53.52  600 :hr (141 :max 145))	; 614.4
  (:warm 5:55.51 1000 :hr (149 :max 154))	;1013.0
  ;pause 1:54.67    0 :hr (127 :max 147 :min 116)
  (:warm 6:32.80 1200 :hr (154 :max 164))
  ;pause 0:15.15    0 :hr (159 :max 161 :min 158)
  (:work 4:59.37 1000 :hr (167 :max 171))	;1047.4
  (:work 4:58.48 1000 :hr (169 :max 173))	;1034.0
  (:work 4:58.50 1000 :hr (170 :max 173))	;1040.4
  (:work 4:56.25 1000 :hr (171 :max 173))	;1033.1
  (:rest 2:07.85    0 :hr (140 :max 171 :min 121 :end 127))
  (:work 4:59.40 1000 :hr (164 :max 171))	;1022.2
  (:work 4:58.67 1000 :hr (170 :max 173))	;1039.4
  (:work 4:57.58 1000 :hr (172 :max 175))	;1019.4
  (:work 2:46.80  600 :hr (178 :max 181))	; 614.7 +
  (:work 1:29.80  400 :hr (185 :max 187))	; 417.1 = 1031.8
  ;pause 9:51.29
  (:cool 5:23.90 .893 :hr (145 :max 153)))

(defworkout "2009-09-25" "14.56"
  (:warm 3:33.39 .545 :hr (137 :max 142))
  (:warm 3:49.80  600 :hr (143 :max 146))
  (:warm 6:05.20 1000 :hr (148 :max 151)) ;1004.0
  (:warm 5:42.32 1000 :hr (155 :max 163)) ;1010.2
  (:warm 5:07.03 1000 :hr (168 :max 176)) ;1018.0
  ;pause 6:41.65           139      172
  (:warm 0:21.65  100 :hr (152 :max 163)) ; 101.8
  ;pause 1:58.91
  (:warm 0:20.81  100 :hr (150 :max 164)) ; 110.0
  ;pause 1:59.38
  (:warm 0:19.47  100 :hr (151 :max 165)) ; 104.5
  ;pause 2:36.90
  (:work 4:12.78 1000 :hr (179 :max 187)) ;1038.4
  (:rest 1:58.85  200 :hr (172 :max 185 :end 153))
  (:work 4:09.91 1000 :hr (182 :max 188)) ;1043.2
  (:rest 1:17.10  200 :hr (179 :max 186 :end 172))
  (:rest 0:42.49    0 :hr (167 :max 172 :end 160))
  (:work 4:09.42 1000 :hr (182 :max 188)) ;1033.5
  (:rest 2:01.20  200 :hr (177 :max 187 :end 161))
  (:work 3:42.70  900 :hr (182 :max 189)) ; 937.8
  (:work 0:18.83  100 :hr (189 :max 189)) ; 105.4
  (:rest 2:02.85  200 :hr (177 :max 189 :end 157))
  ;pause 5:16.76           129      156
  (:cool 3:04.58 .496 :hr (152 :max 159)))

(defworkout "2009-09-28" "16.13"
  (:warm 8:15.59 1.00 :hr (126 :max 132))
  (:warm 6:28.12 1.00 :hr (138 :max 143))
  (:warm 5:48.88 1.00 :hr (146 :max 153))
  (:warm 5:41.87 1.00 :hr (147 :max 160))
  (:warm 5:31.87 1.00 :hr (148 :max 161))
  (:warm 5:28.12 1.00 :hr (154 :max 165))
  (:warm 0:32.51 .099 :hr (154 :max 165))
  (:warm 3:27.68 .585 :hr (151 :max 162))
  (:work 5:01.97 1000 :hr (164 :max 169))
  (:work 4:58.45 1000 :hr (167 :max 170))
  (:work 4:59.39 1000 :hr (167 :max 169))
  (:rest 3:21.54  600 :hr (160 :max 168 :min 157))
  (:work 4:57.93 1000 :hr (164 :max 168))
  (:work 5:01.49 1000 :hr (167 :max 169))
  (:rest 3:18.83  600 :hr (157 :max 167 :min 153))
  (:work 4:59.90 1000 :hr (166 :max 170))
  (:work 5:00.97 1000 :hr (168 :max 170))
  (:work 4:59.22 1000 :hr (169 :max 171))
  (:work 0:29.83  100 :hr (167 :max 168))
  (:work 4:29.03  900 :hr (169 :max 172))
  (:rest 3:19.16  600 :hr (162 :max 170 :min 157))
  (:work 4:56.27 1000 :hr (168 :max 171))
  (:work 4:47.82 1000 :hr (170 :max 173))
  (:work 4:37.63 1000 :hr (173 :max 175))
  (:cool 6:05.06 1.00 :hr (160 :max 173 :min 154))
  (:cool 6:26.44 1.00 :hr (150 :max 157 :min 147)))

(defworkout "2009-09-30" "16.09"
  (:warm 4:00.84 .591)
  (:warm 2:35.42  400)
  (:warm 6:10.73 1000) ;1024.7
  (:warm 5:45.73 1000) ;1030.6
  ;pause 3:36.47
  (:warm 5:28.07 1000) ;1010.8
  (:work 4:57.53 1000) ;1041.4
  (:work 4:48.95 1000) ;1041.1
  (:work 4:38.45 1000) ;1028.3
  (:work 4:20.40 1000) ;1036.2
  (:cool 1:44.57  300)
  ;pause 2:44.31
  (:cool 3:17.20 .533))

(defworkout "2009-10-02" "15.59"
  (:warm 6:29.15 1.00)
  (:warm 6:09.64 1.00)
  (:work 5:48.88 1.00)
  (:work 2:54.64 .526)
  (:work 5:25.82 1.00)
  (:work 5:34.93 1.00)
  (:work 4:12.58 .772)
  ;pause 3:24.57
  (:hill 0:09.93   55) ;61.5
  (:rest 1:58.65    0)
  (:hill 0:09.25   55) ;59.6
  (:rest 2:02.75    0)
  (:hill 0:08.67   55) ;54.2
  ;pause 3:04.18
  (:cool 5:42.64 1.00)
  (:cool 0:50.31 .133))

(defworkout "2009-10-05" "16.00"
  (:warm 3:59.62 .585 :hr (127 :max 134))
  (:warm 2:37.24  400 :hr (132 :max 137))
  (:warm 6:13.03 1000 :hr (138 :max 145))
  (:warm 5:30.07 1000 :hr (148 :max 163))
  ;pause 3:48.25           120/162\108
  (:warm 0:24.55  100 :hr (129 :max 147))
  ;pause 0:36.58           145/151\137
  (:warm 0:25.46  100 :hr (142 :max 150))
  ;pause 1:01.26           139/154\119
  (:work 4:19.61 1000 :hr (166 :max 176))
  (:rest 2:00.77  200 :hr (155 :max 174 :end 120))
  (:work 4:19.52 1000 :hr (168 :max 178))
  (:rest 2:01.53  200 :hr (157 :max 174 :min 126))
  (:work 4:20.40 1000 :hr (169 :max 178))
  (:rest 2:00.87  200 :hr (154 :max 174 :end 122)))

(defworkout "2009-10-07" "16.38"
  (:warm 4:05.51 .610 :hr (128 :max 136))
  (:warm 2:28.00  400 :hr (135 :max 139))
  (:warm 5:54.85 1000 :hr (141 :max 147))
  (:warm 5:23.92 1000 :hr (155 :max 165))
  (:work 4:59.75 1000 :hr (167 :max 170))
  (:work 4:57.34 1000 :hr (167 :max 170))
  ;pause 1:16.79           147/167\129
  (:cool 2:27.65 .429 :hr (145 :max 155)))

(defworkout "2009-10-10" "10.31"
  (:warm 10:48.34 1.648 :hr (143 :max 162))
  ;pause 10:41.62
  (:warm  0:16.56 .043 :hr (133 :max 138))
  ;pause  0:19.94
  (:warm  1:19.73 .253 :hr (151 :max 165)))

(defworkout "2009-10-10" "11.00"
  (:work 5:02.41 1.000 :hr (168 :max 174)) ;1026.1
  (:work 4:55.65 1.000 :hr (172 :max 174)) ;1008.9
  (:work 4:54.45 1.000 :hr (170 :max 173)) ; 999.4
  (:work 4:55.55 1.000 :hr (169 :max 171)) ;1010.7
  (:work 4:51.43 1.000 :hr (169 :max 174)) ;1001.5
  (:work 4:53.17 1.000 :hr (171 :max 173)) ; 986.5
  (:work 4:55.35 1.000 :hr (170 :max 174)) ;1014.4
  (:work 4:50.77 1.000 :hr (171 :max 174)) ;1000.1
  (:work 9:37.33 2.000 :hr (172 :max 177)) ;2009.5
  (:work 4:58.50 1.000 :hr (170 :max 173)) ;1031.7 - vändpunkt
  (:work 4:48.59 1.000 :hr (171 :max 173)) ; 998.9
  (:work 4:56.76 1.000 :hr (173 :max 176)) ;1004.6
  (:work 4:46.78 1.000 :hr (172 :max 176)) ;1003.3
  (:work 4:54.68 1.000 :hr (171 :max 174)) ; 994.9
  (:work 4:43.09 1.000 :hr (171 :max 173)) ;1005.8
  (:work 5:00.93 1.000 :hr (172 :max 174)) ;1007.2
  (:work 4:52.57 1.000 :hr (172 :max 175)) ; 998.8
  (:work 4:53.48 1.000 :hr (173 :max 175)) ;1002.9
  (:work 4:55.97 1.000 :hr (173 :max 176)) ;1005.3
  (:work 4:34.69 .9975 :hr (177 :max 179)) ; 996.9
  (:work 0:21.83   100 :hr (179 :max 179)))

(defworkout "2009-10-14" "16.33"
  (:warm 6:29.20 1.00 :hr (124 :max 132))
  (:warm 6:07.67 1.00 :hr (137 :max 140))
  (:work 6:21.42 1.00 :hr (145 :max 151))
  (:work 6:04.28 1.00 :hr (148 :max 153))
  (:work 5:40.66 1.00 :hr (145 :max 153))
  (:work 5:27.41 1.00 :hr (146 :max 150))
  (:work 5:28.18 1.00 :hr (141 :max 149))
  (:work 5:09.06 .901 :hr (146 :max 149)))

(defworkout "2009-10-16" "16.05"
  (:warm 3:31.16 .555)
  (:warm 3:48.55  600)
  (:warm 5:58.68 1000)
  (:work 6:48.30 1200)
  ;pause 1:30.55
  (:work 5:34.80 1000)
  (:work 5:29.02 1000)
  (:work 4:58.35 1000)
  (:work 0:45.45  200)
  ;pause 3:11.00
  (:work 0:19.88  100)
  (:rest 2:12.17    0)
  (:work 0:18.43  100)
  (:rest 2:19.62    0)
  (:work 0:18.03  100)
  (:rest 2:22.25    0)
  (:work 0:17.07  100)
  (:rest 2:27.85    0)
  (:work 0:16.13  100)
  ;pause 2:39.45
  (:cool 2:26.75 .423))

(defworkout "2009-10-19" "15.20"
  (:warm 6:54.64 1.046)
  (:warm 6:46.25 1.00)
  (:work 6:09.05 1.00)
  (:work 5:42.82 1.00)
  (:work 5:27.26 1.00)
  (:work 4:40.67 .870)
  ;pause 0:13.72 ;37.1
  ;pause 0:34.16
  ;pause 0:24.49 ;39.8
  ;pause 0:22.58
  (:hill 0:08.93 .040) ;46.2
  (:rest 1:21.07    0)
  (:hill 0:08.65 .040) ;42.4
  (:rest 1:25.25    0)
  (:hill 0:08.40 .040) ;44.7
  (:rest 1:36.27    0)
  (:hill 0:07.93 .040) ;43.1
  (:rest 1:48.17    0)
  (:hill 0:07.23 .040) ;40.3
  ;pause 0:51.35
  (:cool 2:55.43 .501))

(defworkout "2009-10-21" "15.30"
  (:warm 2:28.99 .440 :hr (125 :max 141))
  (:warm 2:34.62  400 :hr (137 :max 145))
  (:warm 5:56.93 1000 :hr (144 :max 152)) ;1002.3
  (:warm 5:23.37 1000 :hr (156 :max 163)) ;1000.1
  (:work 5:08.72 1000 :hr (164 :max 169)) ;1023.6
  (:work 4:55.91 1000 :hr (169 :max 171)) ;1024.6
  (:work 4:49.75 1000 :hr (172 :max 174)) ;1036.3
  (:work 4:48.95 1000 :hr (173 :max 175)) ;1032.1
  (:work 4:39.19 1000 :hr (178 :max 182)) ;1025.2
  (:work 0:38.43  200 :hr (186 :max 187)) ; 200.3
  (:cool 2:45.53  500 :hr (174 :max 187 :end 167)) ; 523.7
  (:cool 2:01.20 .331 :hr (162 :max 167 :end 159)))

(defworkout "2009-10-23" "14.35"
  (:warm 3:17.69 .473)
  (:warm 2:37.27  400) ; 404.2
  (:warm 6:06.88 1000) ;1022.0
  (:work 5:45.72 1000) ;1017.5
  (:work 4:13.78  800) ; 810.1
  ;pause 2:37.12
  (:work 5:30.48 1000) ;1011.8
  (:work 5:11.27 1000) ;1013.6
  (:work 3:53.10  800) ; 812.2
  ;pause 7:07.60
  (:work 0:10.18   60) ;  67.1
  (:rest 2:01.12    0)
  (:work 0:10.28   60) ;  61.5
  (:rest 2:09.85    0)
  (:work 0:09.67   60) ;  66.0
  (:rest 1:59.43    0)
  (:work 0:09.35   60) ;  64.0
  (:rest 4:22.17    0)
  (:work 0:09.83   60) ;  67.5
  (:rest 2:03.40    0)
  (:work 0:09.22   60) ;  56.4
  (:rest 2:32.80    0)
  (:work 0:08.25   60) ;  55.7
  ;pause 3:48.88
  (:cool 4:02.73 .679))

(defworkout "2009-10-26" "15.06"
  (:warm 6:46.44 1.024)
  (:warm 6:02.99 1.00)
  (:work 6:03.45 1.00)
  (:work 5:50.33 1.00)
  (:work 5:44.88 1.00)
  (:work 5:25.15 1.00)
  (:work 4:58.05 1.00)
  (:work 1:15.27 .222)
  ;pause 2:33.15
  (:hill 0:07.68   40) ;44.6
  (:rest 2:31.55    0)
  (:hill 0:07.70   40) ;37.7
  (:rest 2:30.95    0)
  (:hill 0:07.45   40) ;47.0
  (:rest 2:30.60    0)
  (:hill 0:07.40   40) ;48.0
  (:rest 3:31.87    0)
  (:hill 0:07.53   40) ;41.6
  (:rest 2:58.05    0)
  (:hill 0:07.60   40) ;44.1
  ;pause 1:35.95
  (:cool 2:52.71 .464))

(defworkout "2009-10-28" "14.56"
  (:warm 2:57.84 .431 :hr (128 :max 136))
  (:warm 3:47.15  600 :hr (136 :max 140))
  (:warm 6:03.75 1000 :hr (143 :max 148))
  (:warm 5:39.35 1000 :hr (151 :max 160))
  (:work 5:02.75 1.00 :hr (163 :max 168))
  (:work 5:36.17 1.00 :hr (168 :max 171))
  (:work 5:12.04 1.00 :hr (170 :max 176))
  (:work 5:06.50 1.00 :hr (169 :max 175))
  (:work 5:07.56 1.00 :hr (169 :max 174))
  (:work 4:47.84 1.00 :hr (170 :max 174))
  (:cool 5:07.15 1.00 :hr (158 :max 172 :min 148))
  (:cool 0:48.09 .126 :hr (156 :max 158)))

(defworkout "2009-10-30" "14.38"
  (:warm 2:57.21 .429)
  (:warm 3:48.28  600) ; 599.7
  (:warm 5:56.22 1000) ; 999.0
  (:work 5:37.43 1000) ;1012.2
  (:work 3:14.62  600) ; 603.2
  ;pause 2:49.00
  (:work 5:39.20 1000) ;1028.2
  (:work 5:17.03 1000) ;1023.6
  (:work 3:57.92  800) ; 821.6
  ;pause 6:54.50
  (:work 0:20.78  100) ; 103.4
  (:rest 2:19.97    0)
  (:work 0:44.88  200) ; 203.9
  (:rest 2:59.75    0)
  (:work 0:42.42  200) ; 199.6
  (:rest 2:58.83    0)
  (:work 0:44.45  220) ; 217.7
  ;pause 4:54.07
  (:work 0:10.03   60) ;  62.4
  (:rest 2:10.10    0)
  (:work 0:09.50   60) ;  67.7
  (:rest 5:06.05    0)
  (:work 0:09.15   60) ;  58.0
  ;pause 3:02.70
  (:cool 2:25.78 .424))

(defworkout "2009-11-01" "15.45"
  (:warm 6:42.12 1.00)
  (:warm 6:27.27 1.00)
  (:work 5:39.59 1.00)
  (:work 5:47.12 1.00)
  (:work 5:06.76 1.00)
  (:work 2:09.01 .412))

(defworkout "2009-11-02" "14.49"
  (:warm 6:35.19 1.00)
  (:warm 6:16.05 1.00)
  (:work 5:43.49 1.00)
  (:work 5:36.64 1.00)
  (:work 5:10.27 1.00)
  (:work 1:23.60 .242)
  (:work 0:14.00   40) ;42.3
  ;pause 2:05.97
  (:hill 0:07.80   40) ;45.0
  (:rest 2:10.94    0)
  (:hill 0:07.54   40) ;41.2
  (:rest 2:11.30    0)
  (:hill 0:07.35   40) ;44.6
  (:rest 2:12.32    0)
  (:hill 0:07.30   40) ;38.5
  (:rest 3:00.40    0)
  (:hill 0:07.53   40) ;41.6
  (:rest 2:59.05    0)
  (:hill 0:07.30   40) ;46.6
  (:rest 3:07.05    0)
  (:hill 0:08.12   40) ;40.9
  ;pause 1:30.48
  (:cool 2:19.53 .370))

(defworkout "2009-11-04" "14.17"
  (:warm 3:10.04 .442 :hr (132 :max 141))
  (:warm 6:08.50 1000 :hr (138 :max 145)) ;1003.9
  (:warm 5:45.85 1000 :hr 149)            ;1013.7
  (:warm 4:11.82  800 :hr (156 :max 162)) ; 809.6
  ;pause 1:42.43           136 :max 160 :min 123
  (:warm 0:20.69  100 :hr (143 :max 154)) ; 100.4
  ;pause 1:45.28           135 :max 159 :min 118
  (:warm 0:18.73  100 :hr (136 :max 154)) ;  96.9
  ;pause 1:49.93           137 :max 161 :min 116
  (:warm 0:19.32  100 :hr (138 :max 156)) ; 107.7
  ;pause 1:14.92           144 :max 160 :min 120
  (:work 5:47.68 1200 :hr (164 :max 171)) ;1239.6
  (:rest 0:30.60    0 :hr (164 :max 171 :end 156))
  (:work 5:46.67 1200 :hr (169 :max 175)) ;1241.6
  (:rest 0:35       0 :hr (169 :max 175 :end 159))
  (:work 5:47.27 1200 :hr (171 :max 176)) ;1232.8
  (:rest 0:31.58    0 :hr (172 :max 175 :end 167))
  (:work 5:47.87 1200 :hr (172 :max 175)) ;1244.1
  (:rest 0:30.78    0 :hr (172 :max 177 :end 168))
  (:work 5:46.92 1200 :hr (172 :max 175)) ;1234.5
  (:rest 0:30.63    0 :hr (173 :max 176 :end 170))
  (:work 5:47.67 1200 :hr (172 :max 177)) ;1247.0
  (:cool 2:38.91 .442 :hr (164 :max 174 :end 159)))

(defworkout "2009-11-06" "14.46"
  (:warm 3:45.09 .567)
  (:warm 6:07.30 1000) ;1009.0
  (:warm 5:46.52 1000) ;1015.0
  (:work 6:35.13 1200) ;1218.4
  ;pause 3:03.92
  (:work 5:27.93 1000) ;1014.6
  (:work 5:25.42 1000) ;1027.5
  (:work 4:05.85  800) ; 803.1
  ;pause 7:03.28
  (:work 0:09.41   60) ;  60.1
  (:rest 3:03.12    0)
  (:work 0:08.97   60) ;  61.8
  (:rest 3:32.10    0)
  (:work 0:08.72   60) ;  57.2
  ;pause 2:53.65
  (:cool 3:41.58 .635))

(defworkout "2009-11-08" "11.12"
  (:warm 6:27.51 1.00)
  (:warm 5:56.59 1.00)
  (:work 5:41.81 1.00)
  (:work 5:40.34 1.00)
  (:work 5:37.75 1.00)
  (:work 5:36.68 1.00)
  (:work 5:34.58 1.00)
  (:work 5:31.00 1.00)
  (:work 5:13.46 1.00)
  (:work 5:13.18 1.00)
  (:work 5:37.97 1.00)
  (:cool 2:52.92 .485))

(defworkout "2009-11-11" "14.06"
  (:warm 3:19.32 .481 :hr (136 :max 141))
  (:warm 6:07.37 1000 :hr (146 :max 151)) ;1026.8
  (:warm 5:48.33 1000 :hr (155 :max 158)) ;1009.1
  (:warm 4:32.50  800 :hr (160 :max 164)) ; 818.8
  ;pause 2:00.50      :hr (135 :max 161 :min 123)
  (:warm 0:24.00  100 :hr (142 :max 152)) ; 107.4
  ;pause 1:47.97      :hr (135 :max 158 :min 120)
  (:warm 0:21.10  100 :hr (142 :max 157)) ; 106.8
  ;pause 1:52.73      :hr (137 :max 164 :min 118)
  (:warm 0:18.52  100 :hr (147 :max 163)) ; 106.6
  ;pause 1:30.98      :hr (149 :max 168 :min 132)
  (:work 2:03.62  400 :hr (160 :max 172)) ; 416.1
  (:work 1:52.90  400 :hr (173 :max 175)) ; 421.2
  (:work 2:03.57  400 :hr (172 :max 174)) ; 412.0
  (:work 1:53.56  400 :hr (175 :max 177)) ; 412.3
  (:work 2:04.29  400 :hr (173 :max 177)) ; 416.3
  (:work 1:53.13  400 :hr (175 :max 177)) ; 418.2
  (:work 2:03.10  400 :hr (174 :max 177)) ; 411.3
  (:work 1:52.13  400 :hr (175 :max 177)) ; 410.4
  (:work 2:04.54  400 :hr (172 :max 176)) ; 408.7
  (:work 1:51.81  400 :hr (174 :max 176))
  (:cool 1:08.32  200 :hr 173)
  (:cool 1:57.82 .334 :hr (163 :max 168 :min 160)))

(defworkout "2009-11-13" "14.05"
  (:warm 4:05.79 .599)
  (:warm 6:07.32 1000) ;1027.2
  (:warm 5:48.86 1000) ;1029.0
  (:warm 2:15.75  400) ; 410.0
  ;pause 2:44.50
  (:warm 6:27.62 1200) ;1238.0
  ;pause 2:03.78
  (:warm 0:23.12  100) ; 104.3
  ;pause 2:00.80
  (:work 0:44.94  200) ; 207.0
  (:rest 2:48.89    0)
  (:work 0:43.03  200) ; 205.9
  (:rest 2:50.15    0)
  (:work 0:41.93  200) ; 210.0
  (:rest 3:06.44    0)
  (:work 0:38.11  200) ; 199.7
  ;pause 1:45.45
  (:cool 3:22.80 .636))

(defworkout "2009-11-15" "15.13"
  (:warm 3:31.49 .499)
  (:warm 6:04.93 1.00)
  (:work 5:47.30 1.00)
  (:work 5:36.63 1.00)
  (:work 5:51.09 1.00)
  (:work 5:38.64 1.00)
  (:work 5:27.88 1.00)
  (:work 5:42.92 1.00)
  (:work 5:22.99 1.00)
  (:work 5:12.52 1.00)
  (:work 5:22.22 1.00)
  (:work 5:39.14 1.00)
  (:work 5:44.21 1.00)
  (:work 3:47.29 .634))

(defworkout "2009-11-16" "15.57"
  (:warm 3:35.67 .516)
  (:warm 6:14.10 1.00)
  (:work 5:51.48 1.00)
  (:work 5:53.36 1.00)
  (:work 5:23.80 1.00)
  (:work 5:09.50 1.00)
  (:work 0:52.81 .173)
  ;pause 1:03.72
  (:hill 0:08.37   40)
  (:rest 2:51.53    0)
  (:hill 0:07.48   40)
  (:rest 2:56.05    0)
  (:hill 0:07.67   40)
  (:rest 2:55.65    0)
  (:hill 0:07.56   40)
  (:rest 2:59.72    0)
  (:hill 0:07.35   40)
  (:rest 3:00.60    0)
  (:hill 0:07.47   40)
  (:rest 3:06.22    0)
  (:hill 0:07.41   40)
  ;pause 1:52.60
  (:cool 2:52.40 .466))

(defworkout "2009-11-18" "14.57"
  (:warm 3:37.09 .511 :hr (133 :max 149))
  (:warm 6:08.13 1000 :hr (142 :max 148)) ;1016.2
  (:warm 5:50.20 1000 :hr (150 :max 154)) ;1023.4
  (:warm 5:37.10 1000 :hr (156 :max 162)) ;1025.4
  (:work 5:12.40 1000 :hr (166 :max 171)) ;1032.0
  (:work 0:03      10 :hr 167)
  (:work 0:57.16  190 :hr (167 :max 169)) ; 203.5
  (:work 4:06.85  800 :hr (170 :max 172)) ; 830.8
  (:work 5:02.68 1000 :hr (171 :max 172)) ;1033.1
  (:work 4:59.45 1000 :hr (172 :max 175)) ;1042.0
  (:work 4:50.55 1000 :hr (175 :max 177)) ;1023.3
  (:work 0:03      10 :hr 175)
  (:work 0:26.02   90 :hr (175 :max 169)) ;  93.2
  (:work 4:20.82  900 :hr (174 :max 177)) ; 933.5
  (:cool 5:56.76 1000 :hr (163 :max 175 :end 157)) ;1041.4
  (:cool 3:27.63 .562 :hr (156 :max 159 :min 153))) 

(defworkout "2009-11-20" "14.20"
  (:warm 3:58.89 .594)
  (:warm 6:05.53 1000) ;1020.1
  (:work 5:39.72 1000) ;1021.2
  (:work 6:38.00 1200) ;1239.2
  ;pause 2:40.00
  (:work 5:29.07 1000) ;1006.8
  (:work 5:14.66 1000) ;1010.4
  ;pause 2:01.00
  (:work 0:23.12  100) ; 108.5
  (:rest 2:05.83    0)
  (:work 0:20.35  100) ; 103.1
  (:rest 2:16.82    0)
  (:work 0:44.90  200) ; 208.7
  (:rest 3:01.35    0)
  (:work 0:40.28  200) ; 200.4
  (:rest 2:57.03    0)
  (:work 0:40.97  200) ; 208.8
  (:rest 3:00.00    0)
  (:work 0:38.27  200) ; 209.5
  (:rest 3:07.43    0)
  (:work 0:39.99  200) ; 216.1
  ;pause 3:48.11
  (:cool 2:25.30 .408))

(defworkout "2009-11-22" "10.49"
  (:warm 6:40.40 1.00 :hr (151 :max 174))
  (:warm 6:02.97 1.00 :hr (150 :max 158))
  (:work 5:48.49 1.00 :hr (154 :max 158))
  (:work 5:42.42 1.00 :hr (159 :max 167))
  (:work 6:19.56 1.00 :hr (165 :max 170))
  (:work 5:09.67 1.00 :hr (158 :max 164))
  (:work 5:22.27 1.00 :hr (155 :max 162))
  (:work 5:39.16 1.00 :hr (157 :max 160))
  (:work 5:42.64 1.00 :hr (155 :max 161))
  (:work 5:43.33 1.00 :hr (156 :max 160))
  (:work 5:46.20 1.00 :hr (153 :max 156))
  (:work 5:38.73 1.00 :hr (154 :max 158))
  (:work 5:40.62 1.00 :hr (160 :max 168))
  (:work 5:35.87 1.00 :hr (164 :max 172))
  (:cool 3:34.39 .573 :hr (155 :max 159)))

(defworkout "2009-11-24" "14.51"
  (:warm  3:31.36  .522)
  (:warm  6:01.13  1000)
  (:work  5:39.07  1000)
  (:work  5:32.76  1000)
  (:work 10:44.80 1.885)
  ;pause  1:03.72
  (:hill  0:07.65    40)
  (:rest  2:51.53     0)
  (:hill  0:07.42    40)
  (:rest  2:55.93     0)
  (:hill  0:07.30    40)
  (:rest  2:57.52     0)
  (:hill  0:07.48    40)
  (:rest  3:07.80     0)
  (:hill  0:07.27    40)
  (:rest  3:13.68     0)
  (:hill  0:07.35    40)
  (:rest  3:18.17     0)
  (:hill  0:07.40    40)
  ;pause  1:25.93
  (:cool  3:03.38  .476))

(defworkout "2009-11-25" "15.01"
  (:warm 4:08.97 .612 :hr (129 :max 138))
  (:warm 6:15.23 1000 :hr (139 :max 152))	   ;1024.0
  (:warm 5:49.12 1000 :hr (146 :max 150))	   ;1020.3
  (:warm 5:39.74 1000 :hr (152 :max 157))	   ;1017.2
  (:warm 4:24.51  800 :hr (159 :max 164))	   ; 815.1
  (:warm 0:56.52  200 :hr (166 :max 169))	   ; 200.6
  ;pause 0:59.43      :hr (156 :max 168 :end 139)
  (:warm 0:22.30  100 :hr (148 :max 157))          ;  98.3
  (:rest 0:46.16    0 :hr (157 :max 162 :end 141))
  (:warm 0:21.96  100 :hr (149 :max 158))          ;  98.8
  ;pause 0:56.80      :hr (158 :max 169 :end 159)
  (:work 1:50.28  400 :hr (163 :max 174))          ; 410.6
  (:rest 0:45.92  100 :hr (169 :max 174 :end 163)) ; 111.1
  (:work 1:50.23  400 :hr (169 :max 174))          ; 396.5
  (:rest 0:46.12  100 :hr (170 :max 174 :end 166)) ; 107.2
  (:work 1:48.92  400 :hr (170 :max 174))          ; 411.4
  (:rest 0:46.31  100 :hr (170 :max 174 :end 163)) ; 109.3
  (:work 1:49.60  400 :hr (171 :max 175))          ; 399.7
  (:rest 0:46.28  100 :hr (170 :max 175 :end 163)) ; 105.3
  (:work 1:47.12  400 :hr (170 :max 176))          ; 410.3
  (:rest 0:47.14  100 :hr (169 :max 174 :end 163)) ; 106.3
  (:work 1:48.86  400 :hr (171 :max 176))          ; 401.0
  (:rest 0:46.02  100 :hr (171 :max 175 :end 166)) ; 107.4
  (:work 1:44.40  400 :hr (174 :max 180))          ; 413.0
  (:rest 0:46.88  100 :hr (172 :max 177 :end 165)) ; 108.6
  (:work 1:42.42  400 :hr (174 :max 179))          ; 398.9
  (:rest 0:45.67  100 :hr (172 :max 177 :end 167)) ; 107.2
  ;pause 1:01.01      :hr (154 :max 167 :end 140)
  (:cool 5:31.95 1000 :hr (156 :max 163 :end 156))
  (:cool 1:57.45  335 :hr (154 :max 157 :min 150)) ; 334.8
  (:cool 0:21.55   65 :hr 155)
  (:cool 3:29.43  600 :hr (155 :max 160 :end 152)) ; 625.1
  (:cool 2:42.34 .434 :hr (156 :max 163 :min 148)))

(defworkout "2009-11-27" "15.00"
  (:warm 3:57.52 .611)
  (:warm 6:09.37 1000) ;1024.0
  (:warm 5:47.63 1000) ;1016.3
  (:warm 5:41.52 1000) ;1027.8
  (:warm 5:21.15 1000) ;1015.2
  ;pause 4:00.00
  (:warm 0:21.18  100) ; 101.2
  (:rest 1:39.52    0)
  (:warm 0:20.13  100) ; 103.0
  ;pause 3:07.75
  (:work 1:09.90  300) ; 307.8
  (:rest 3:34.80    0)
  (:work 1:06.42  300) ; 302.0
  (:rest 3:59.88    0)
  (:work 1:07.17  300) ; 309.0
  (:rest 4:08.81    0)
  (:work 0:59.79  300) ; 306.0
  ;pause 2:05.75
  (:cool 5:37.90 1000) ;1038.8
  (:cool 5:48.73 1000) ;1021.2
  ;pause 0:35.37
  (:cool 2:34.93 .426))

(defworkout "2009-11-29" "9.39"
  (:warm 3:34.12 .501)
  (:warm 6:12.54 1.00)
  (:work 5:48.26 1.00)
  (:work 5:48.84 1.00)
  (:work 5:50.54 1.00)
  (:work 6:01.31 1.00)
  (:work 5:02.49 1.00)
  (:work 5:39.03 1.00)
  (:work 5:41.66 1.00)
  (:work 5:41.22 1.00)
  (:work 5:40.58 1.00)
  (:work 5:34.45 1.00)
  (:work 5:31.68 1.00)
  (:work 5:23.51 1.00)
  (:work 5:30.57 1.00)
  (:work 5:23.40 1.00)
  (:cool 4:20.62 .721))

(defworkout "2009-12-02" "13.30"
  (:warm 3:31.88 .502 :hr (128 :max 134))
  (:warm 6:16.10 1.00 :hr (139 :max 144))
  (:warm 5:51.25 1.00 :hr (148 :max 150))
  (:work 5:40.67 1.00 :hr (153 :max 161))
  (:work 5:30.42 1.00 :hr (156 :max 159))
  (:work 5:18.93 1.00 :hr (162 :max 164))
  (:work 5:08.06 1.00 :hr (165 :max 170))
  (:work 4:57.07 1.00 :hr (170 :max 174))
  (:work 4:46.19 1.00 :hr (174 :max 175))
  (:cool 4:10.00 .686 :hr (162 :max 174 :min 149 :end 155)))

(defworkout "2009-12-04" "10.22"
  (:warm 3:28.17 .503)
  (:warm 6:06.17 1.00)
  (:work 5:44.73 1.00)
  (:work 5:45.29 1.00)
  (:work 5:39.97 1.00)
  (:work 5:32.51 1.00)
  (:work 5:35.91 1.00)
  (:work 5:32.89 1.00)
  (:work 5:33.55 1.00)
  (:rest 1:26.98    0)
  (:work 0:21.82  100) ; 96.7
  (:rest 1:38.45    0)
  (:work 0:19.25  100) ;101.2
  (:rest 1:47.52    0)
  (:work 0:19.31  100) ;104.1
  (:rest 1:40.22    0)
  (:work 0:17.85  100) ;105.2
  (:rest 2:04.10    0)
  (:work 0:15.33  100) ;105.5
  (:rest 0:58.62    0)
  (:cool 4:28.23 .754))

(defworkout "2009-12-06" "12.02"
  (:warm 3:23.17 .506)
  (:warm 6:05.03 1.00)
  (:work 5:42.18 1.00)
  (:work 5:34.34 1.00)
  (:work 5:46.52 1.00)
  (:work 5:49.43 1.00)
  (:work 5:00.22 1.00)
  (:work 5:30.84 1.00)
  (:work 5:40.93 1.00)
  (:work 5:32.75 1.00)
  (:work 5:36.30 1.00)
  (:work 5:28.02 1.00)
  (:work 5:24.15 1.00)
  (:work 5:23.27 1.00)
  (:cool 5:50.09 1.00)
  (:cool 2:00.45 .331))

(defworkout "2009-12-07" "11.46"
  (:warm 3:11.82 .505)
  (:warm 5:53.92 1.00)
  (:work 5:34.75 1.00)
  (:work 5:29.34 1.00)
  (:work 5:28.24 1.00)
  (:work 5:23.96 1.00)
  (:work 2:04.36 .375)
  ;pause 3:23.24
  (:hill 0:05.64   33) ;28.6
  (:rest 1:48.57    0)
  (:hill 0:05.73   33) ;33.2
  (:rest 2:07.56    0)
  (:hill 0:05.71   33) ;32.0
  (:rest 2:02.64    0)
  (:hill 0:05.55   33) ;27.6
  (:rest 2:10.57    0)
  (:hill 0:05.84   33) ;18.5
  (:rest 2:10.16    0)
  (:hill 0:05.68   33) ;19.6
  (:rest 2:00.90    0)
  (:hill 0:05.67   33) ;31.8
  (:rest 2:06.65    0)
  (:hill 0:05.30   33) ;33.7
  ;pause 1:37.75
  (:cool 3:15.91 .559))

(defworkout "2009-12-09" "15.54"
  (:warm 3:14.27  500 :hr (138 :max 148))
  (:warm 6:02.62 1000 :hr (148 :max 153))
  (:warm 5:44.20 1000 :hr (155 :max 160))
  (:warm 5:24.20 1000 :hr (161 :max 164))
  (:warm 1:09.31  200 :hr (162 :max 164))
  ;pause 1:32.24      :hr (139 :max 158)
  (:warm 0:21.85  100 :hr (142 :max 157))
  (:rest 1:01.71    0 :hr (150 :max 165))
  (:warm 0:21.09  100 :hr (142 :max 160))
  (:rest 1:03.39    0 :hr (153 :max 168))
  (:warm 0:20.24  100 :hr (148 :max 164))
  ;pause 5:29.71      :hr (136 :max 171 :end )
  (:work 1:51.24  400 :hr (159 :max 175))
  (:rest 0:41.10  100 :hr (172 :max 175 :end 169))
  (:work 1:48.37  400 :hr (174 :max 177))
  (:rest 0:40.88  100 :hr (174 :max 177 :end 169))
  (:work 1:44.68  400 :hr (174 :max 178))
  (:rest 0:41.62  100 :hr (175 :max 177 :end 171))
  (:work 1:47.60  400 :hr (176 :max 179))
  (:rest 0:42.35  100 :hr (175 :max 178 :end 171))
  (:work 1:46.48  400 :hr (174 :max 179))
  (:rest 0:41.48  100 :hr (175 :max 179 :end 171))
  (:work 1:48.19  400 :hr (175 :max 179))
  (:rest 0:41.50  100 :hr (175 :max 179 :end 172))
  (:work 1:47.95  400 :hr (176 :max 179))
  (:rest 0:41.43  100 :hr (175 :max 178 :end 171))
  (:work 1:49.45  400 :hr (175 :max 178))
  (:rest 0:41.42  100 :hr (175 :max 179 :end 171))
  (:work 1:35.73  400 :hr (179 :max 184))
  (:rest 0:42.03  100 :hr (176 :max 181 :end 168))
  ;pause 1:30.11  100 :hr (147 :max 168 :end 128)
  ;pause 1:10.81      :hr (128 :max 131)
  (:cool 5:38.67 1000 :hr (157 :max 164 :end 159))
  (:cool 6:18.38 1000 :hr (152 :max 161 :min 144 :end 148)))

(defworkout "2009-12-11" "10.16"
  (:warm 3:29.24 .507)
  (:warm 6:03.35 1.00)
  (:work 5:45.05 1.00)
  (:work 5:43.53 1.00)
  (:work 5:31.62 1.00)
  (:work 5:30.03 1.00)
  (:work 3:00.62 .552)
  (:work 0:43     200) ;202.4
  (:rest 3:03.20    0)
  (:work 0:40.75  200) ;204.3
  (:rest 3:01.97    0)
  (:work 0:40.00  200) ;206.5
  (:rest 3:02.88    0)
  (:work 0:41.85  200) ;198.6
  (:rest 3:05.65    0)
  (:work 0:40.72  200) ;198.2
  (:rest 2:00.30    0)
  (:cool 6:02.54 1.00))

(defworkout "2009-12-14" "11.36"
  (:warm 3:16.24 .507)
  (:warm 6:01.74 1.00)
  (:work 5:45.90 1.00)
  (:work 5:39.91 1.00)
  (:work 5:30.23 1.00)
  (:work 5:17.82 1.00)
  (:work 5:17.94 1.00)
  (:rest 4:02.79    0)
  (:hill 0:06.27   40) ;43.4
  (:rest 1:50.33    0)
  (:hill 0:06.47   40) ;38.1
  (:rest 3:04.48    0)
  (:hill 0:06.30   40) ;39.5
  (:rest 2:30.82    0)
  (:hill 0:06.53   40) ;39.5
  (:rest 2:43.95    0)
  (:hill 0:06.27   40) ;45.0
  (:rest 2:49.07    0)
  (:hill 0:06.28   40) ;36.4
  (:rest 2:43.38    0)
  (:hill 0:06.10   40) ;53.5
  (:rest 3:05.00    0)
  (:hill 0:06.20   40) ;57.7
  (:rest 2:34.70    0)
  (:cool 5:49.14 1.00)
  (:cool 1:03.68 .171))

(defworkout "2009-12-16" "15.30"
  (:warm 3:32.92 .506 :hr 130)
  (:warm 6:09.48 1.00 :hr 140)
  (:warm 5:41.82 1.00 :hr (147 :max 158))
  (:warm 5:25.27 1.00 :hr (156 :max 163))
  (:work 5:02.85 1.00 :hr (164 :max 167))
  (:work 5:03.75 1.00 :hr (166 :max 170))
  (:work 5:05.25 1.00 :hr (168 :max 171))
  (:work 5:18.00 1.00 :hr (171 :max 173))
  (:work 5:24.83 1.00 :hr (167 :max 173))
  (:work 5:14.99 1.00 :hr (171 :max 173))
  (:work 4:44.39 1.00 :hr (175 :max 178))
  (:cool 5:14.21 .872 :hr (161 :max 178 :end 152)))

(defworkout "2009-12-26" "12.21"
  (:warm 3:35.98 .506 :hr (128 :max 139))
  (:warm 6:13.59 1.00 :hr (140 :max 150))
  (:work 5:46.54 1.00 :hr (152 :max 156))
  (:work 5:50.83 1.00 :hr (154 :max 161))
  (:work 6:09.75 1.00 :hr (156 :max 166))
  (:work 6:07.08 1.00 :hr (164 :max 169))
  (:work 5:23.37 1.00 :hr (152 :max 158))
  (:work 5:50.69 1.00 :hr (159 :max 165))
  (:work 5:52.23 1.00 :hr (157 :max 161))
  (:work 5:52.15 1.00 :hr (155 :max 159))
  (:work 5:50.03 1.00 :hr (155 :max 159))
  (:work 5:32.44 1.00 :hr (159 :max 162))
  (:work 5:21.44 1.00 :hr (163 :max 168))
  (:work 5:33.93 1.00 :hr (163 :max 168))
  (:work 5:51.75 1.00 :hr (154 :max 165))
  (:cool 0:49.29 .137 :hr (153 :max 154)))

(defworkout "2009-12-28" "16.40"
  (:warm 3:37.09 .503)
  (:warm 6:07.22 1.00)
  (:work 5:44.99 1.00)
  (:work 3:46.37 .699)
  (:work 0:14.59   45) ;43.8
  (:work 0:45.86 .113)
  (:work 0:15.97   45) ;47.8
  (:rest 1:14.23    0)
  (:hill 0:08.57   45) ;57.7
  (:rest 1:15.25    0)
  (:hill 0:08.10   45) ;45.8
  (:rest 1:23.98    0)
  (:hill 0:07.92   45) ;49.6
  (:rest 1:22.52    0)
  (:hill 0:07.56   45) ;47.6
  (:rest 1:22.27    0)
  (:hill 0:07.25   45) ;46.6
  (:rest 1:36.10    0)
  (:work 5:39.49 1.00)
  (:work 5:25.35 1.00)
  (:work 5:03.69 .905))

(defworkout "2009-12-30" "17.02"
  (:warm 3:24.52 .509)
  (:warm 5:57.21 1.00)
  (:work 5:39.39 1.00)
  (:work 5:17.43 1.00)
  (:work 4:58.34 1.00)
  (:work 4:58.97 1.00)
  (:work 4:54.75 1.00)
  (:work 4:52.05 1.00)
  (:work 4:31.58 1.00)
  (:cool 4:18.56 .698))

(defworkout "2010-01-03" "12.08"
  (:warm 6:13.22 1.00)
  (:work 5:41.49 1.00)
  (:work 5:36.95 1.00)
  (:work 5:46.22 1.00)
  (:work 6:33.03 1.00)
  (:work 5:17.83 1.00)
  (:work 5:32.49 1.00)
  (:work 5:34.59 1.00)
  (:work 5:42.40 1.00)
  (:work 5:42.01 1.00)
  (:work 5:37.75 1.00)
  (:work 5:37.63 1.00)
  (:work 5:35.17 1.00)
  (:work 5:42.55 1.00)
  (:work 5:15.23 1.00)
  (:cool 4:02.83 .674))

(defworkout "2010-01-04" "17.25"
  (:warm 6:14.37 1.00)
  (:work 5:40.26 1.00)
  (:work 5:34.82 1.00)
  (:work 0:48.17 .150)
  (:work 0:13.82   45) ;47.1
  (:rest 2:28.05    0)
  (:hill 0:07.65   45) ;47.2
  (:rest 3:03.38    0)
  (:hill 0:07.50   45) ;56.2
  (:rest 3:06.37    0)
  (:hill 0:07.71   45) ;55.8
  (:rest 3:02.19    0)
  (:hill 0:07.55   45) ;59.0
  (:rest 3:09.53    0)
  (:hill 0:07.37   45) ;55.0
  (:rest 3:24.00    0)
  (:hill 0:07.31   45) ;47.4
  (:rest 3:01.44    0)
  (:hill 0:07.56   45) ;48.9
  (:rest 2:42.89    0)
  (:work 5:45.06 1.00)
  (:work 5:18.60 1.00)
  (:work 5:28.34 1.00))

(defworkout "2010-01-06" "16.55"
  (:warm 3:14.4 0.50 :hr (141 :max 148))
  (:warm 3:07.1 0.50 :hr (155 :max 159))
  (:warm 5:50.0 1.00 :hr (164 :max 168))
  (:warm 5:33.9 1.00 :hr (170 :max 172))
  (:work 5:20.3 1.00 :hr (174 :max 176))
  (:work 5:20.2 1.00 :hr (176 :max 179))
  (:work 5:16.6 1.00 :hr (179 :max 182))
  (:work 5:07.2 1.00 :hr (182 :max 184)) ;179
  (:cool 5:49   1.00 :hr (176 :max 184)) ;179
  (:cool 6:24.8 1.00 :hr (173 :max 176 :end 173))
  ;pause 5:17.8      :hr (152 :max 173 :end 146))
  )

(defworkout "2010-01-09" "11.56"
  (:warm 2:40.3 #.(* 2 192))
  (:warm 5:47.6 #.(* 5 192))
  (:work 5:17.1 #.(* 5 192))
  (:work 4:19.2 #.(* 5 192))
  (:work 0:59.2 #.(* 1 192)))

(defworkout "2010-01-10" "13.03"
  (:warm 3:30.79 .503)
  (:warm 6:05.95 1.00)
  (:work 5:39.67 1.00)
  (:work 5:44.08 1.00)
  (:work 5:43.43 1.00)
  (:work 6:25.08 1.00)
  (:work 5:03.49 1.00)
  (:work 5:31.82 1.00)
  (:work 5:30.08 1.00)
  (:work 5:29.16 1.00)
  (:work 5:37.14 1.00)
  (:work 5:24.09 1.00)
  (:work 5:28.72 1.00)
  (:work 5:32.47 1.00)
  (:work 5:29.36 1.00)
  (:work 5:33.98 1.00)
  (:work 5:26.21 1.00))

(defworkout "2010-01-11" "17.50"
  (:warm 3:29.59 .503)
  (:warm 6:07.78 1.00)
  (:work 5:40.76 1.00)
  (:work 5:31.68 1.00)
  (:work 0:20.00 .074)
  (:rest 1:46.1     0)
  (:hill 0:08.8    45)
  (:rest 1:47.9     0)
  (:hill 0:08.5    45)
  (:rest 2:29.4     0)
  (:hill 0:08.2    45)
  (:rest 2:49.8     0)
  (:hill 0:07.9    45)
  (:rest 2:59.3     0)
  (:hill 0:07.8    45)
  (:rest 0:25.2     0)
  (:rest 1:27.18    0)
  (:work 5:37.16 1.00)
  (:work 5:24.97 1.00)
  (:work 5:15.07 1.00))

(defworkout "2010-01-13" "17.34"
  (:warm 3:21.73 .503 :hr (133 :max 137))
  (:warm 6:09.24 1.00 :hr (144 :max 148))
  (:warm 5:42.69 1.00 :hr (152 :max 156))
  (:warm 5:23.71 1.00 :hr (159 :max 166))
  (:work 5:08.97 1.00 :hr (164 :max 167))
  (:work 5:07.12 1.00 :hr (168 :max 171))
  (:work 5:06.29 1.00 :hr (170 :max 175))
  (:work 5:06.85 1.00 :hr (171 :max 173))
  (:work 4:49.55 1.00 :hr (174 :max 178))
  (:cool 4:27.42 .775 :hr (166 :max 176 :end 160)))

(defworkout "2010-01-15" "11.22"
  (:work 37:00 7.0))

(defworkout "2010-01-17" "10.23"
  (:warm 3:19.44 .499)
  (:warm 6:04.68 1.00)
  (:work 5:41.35 1.00)
  (:work 5:41.19 1.00)
  (:work 5:41.49 1.00)
  (:work 6:02.29 1.00)
  (:work 4:47.91 1.00)
  (:work 5:29.67 1.00)
  (:work 5:18.01 1.00)
  (:work 5:29.24 1.00)
  (:work 5:23.38 1.00)
  (:work 5:26.03 1.00)
  (:work 5:19.03 1.00)
  (:work 5:21.82 1.00)
  (:work 5:21.99 1.00)
  (:work 5:35.15 1.00)
  (:work 5:15.71 1.00)
  (:work 2:14.51 .396))

(defworkout "2010-01-18" "16.53"
  (:warm 3:19.19 .502)
  (:warm 6:06.45 1.00)
  (:work 5:50.25 1.00)
  (:work 5:41.58 1.00)
  (:work 0:29.40 .109)
  ;pause 2:30.57
  (:hill 0:08.20   45)
  (:rest 2:54.70    0)
  (:hill 0:07.65   45)
  (:rest 2:58.40    0)
  (:hill 0:07.75   45)
  (:rest 3:03.88    0)
  (:hill 0:07.80   45)
  (:rest 3:04.67    0)
  (:hill 0:07.85   45)
  (:rest 3:11.95    0)
  (:hill 0:07.45   45)
  ;pause 2:34.03
  (:work 5:54.56 1.00)
  (:work 5:43.17 1.00)
  (:work 5:42.80 1.00)
  (:work 1:25.57 .234))

(defworkout "2010-01-20" "11.23"
  (:warm 6:10.86 1.00)
  (:warm 5:41.41 1.00)
  (:warm 5:26.45 1.00)
  (:work 5:02.92 1.00)
  (:work 5:04.19 1.00)
  (:work 4:55.39 1.00)
  (:work 4:54.39 1.00)
  (:work 4:59.12 1.00)
  (:work 4:48.96 1.00)
  (:cool 5:20.78 1.00))

(defworkout "2010-01-21" "16.00"
  (:warm 3:55 600)
  (:warm 1:12 200))

(defworkout "2010-01-22" "16.08"
  (:warm 3:30.42 .555)
  (:warm 5:49.07 1.00)
  (:work 5:30.85 1.00)
  (:work 5:17.39 1.00)
  (:work 5:06.94 1.00)
  (:work 5:11.01 1.00)
  (:work 5:21.41 1.00)
  (:work 4:58.90 .919)
  ;pause 3:08.37
  (:work 5:13.75 1.00)
  (:cool 2:38.39 .464))

(defworkout "2010-01-24" "11.23"
  (:warm 6:21.40 1.00 :hr 144)
  (:work 5:51.42 1.00 :hr 148)
  (:work 5:50.53 1.00 :hr 147)
  (:work 5:49.26 1.00 :hr (150 :max 155))
  (:work 6:29.06 1.00 :hr (158 :max 164))
  (:work 5:05.25 1.00 :hr (152 :max 158))
  (:work 5:39.34 1.00 :hr (154 :max 158))
  (:work 5:47.32 1.00 :hr (150 :max 155))
  (:work 5:51.36 1.00 :hr (150 :max 153))
  (:work 5:49.84 1.00 :hr (151 :max 156))
  (:work 5:45.39 1.00 :hr (153 :max 157))
  (:work 5:54.49 1.00 :hr (150 :max 153))
  (:work 5:49.80 1.00 :hr (151 :max 155))
  (:work 5:55.94 1.00 :hr (152 :max 159))
  (:work 5:40.88 1.00 :hr (155 :max 161))
  (:work 5:45.37 1.00 :hr (150 :max 155))
  (:work 5:44.28 1.00 :hr (150 :max 154))
  (:cool 0:53.20 .146 :hr (154 :max 155)))

(defworkout "2010-01-27" "11.41"
  (:warm 6:05.58 1.00)
  (:warm 5:41.67 1.00)
  (:warm 5:41.84 1.00)
  (:work 5:23.51 1.00)
  (:work 5:14.65 1.00)
  (:work 5:09.62 1.00)
  (:work 5:00.14 1.00)
  (:work 4:45.56 1.00)
  (:cool 4:25.65 .819))

(defworkout "2010-01-29" "16.15"
  (:warm 6:03.8 #.(* 5 192))
  (:warm 5:32.1 #.(* 5 192))
  (:warm 6:27.1 #.(* 6 192))
  (:warm 4:00.9 #.(* 4 192))
  (:warm 4:51.9 #.(* 5 192))
  (:warm 4:36.3 #.(* 5 192))
  ;pause 12:40.8
  (:warm 0:11.9  50)
  ;pause 1:19.6
  (:work 0:46.1 200)
  (:rest 3:07.3   0)
  (:work 0:43.3 200)
  (:rest 3:01.2   0)
  (:work 0:40.8 200)
  (:rest 3:02.8   0)
  (:work 0:38.2 200)
  (:rest 3:20.1   0)
  (:work 0:36.7 200)
  ;pause 5:45.8
  (:cool 5:17.0 #.(* 5 192))
  (:cool 5:12.8 #.(* 5 192))
  (:cool 2:26.0 #.(* 2 192)))

(defworkout "2010-01-31" "10.03"
  (:warm 6:24.49 1.00)
  (:work 5:50.47 1.00)
  (:work 5:43.20 1.00)
  (:work 5:42.03 1.00)
  (:work 6:10.86 1.00)
  (:work 5:08.57 1.00)
  (:work 5:23.11 1.00)
  (:work 5:30.74 1.00)
  (:work 5:35.02 1.00)
  (:work 5:26.96 1.00)
  (:work 5:32.29 1.00)
  (:work 5:28.52 1.00)
  (:work 5:31.88 1.00)
  (:work 5:41.95 1.00)
  (:work 5:28.52 1.00)
  (:cool 3:36.88 .603))

(defworkout "2010-02-01" "16.47"
  (:warm 6:18.69 1.00)
  (:work 5:47.76 1.00)
  (:work 5:33.71 1.00)
  (:work 0:33.68 .106)
  (:work 0:25.28 .085) ;83.1m
  ;pause 2:48.85
  (:hill 0:16.77   85) ;95.1
  (:rest 2:55.28    0)
  (:hill 0:15.50   85) ;93.6
  (:rest 3:00.17    0)
  (:hill 0:15.28   85) ;88.4
  (:rest 2:59.97    0)
  (:hill 0:15.26   85) ;87.5
  ;pause 3:32.51
  (:work 5:39.42 1.00)
  (:work 5:22.72 1.00)
  (:work 5:07.72 .988))

(defworkout "2010-02-03" "10.59"
  (:warm 6:15.52 1.00 :hr (139 :max 149))
  (:warm 5:41.88 1.00 :hr (154 :max 159))
  (:warm 5:36.61 1.00 :hr (164 :max 170))
  (:work 5:19.31 1.00 :hr (169 :max 172))
  (:work 5:15.32 1.00 :hr (168 :max 172))
  (:work 5:21.84 1.00 :hr (170 :max 176))
  (:work 5:08.80 1.00 :hr (171 :max 175))
  (:work 5:04.38 1.00 :hr (170 :max 173))
  (:work 5:02.09 1.00 :hr (175 :max 178))
  (:cool 5:38.08 1.00 :hr (169 :max 177))
  (:cool 1:17.38 .243 :hr (166 :max 167)))

(defworkout "2010-02-05" "14.33"
  (:warm 6:11.0 #.(* 5 192))
  (:work 5:34.4 #.(* 5 192))
  (:work 5:26.1 #.(* 5 192))
  (:work 5:24.8 #.(* 5 192))
  (:work 5:23.5 #.(* 5 192))
  (:work 4:56.7 #.(* 5 192))
  ;pause 10:48.0
  (:work 0:21.3        100)
  (:rest 1:36.4          0)
  (:work 0:21.5        100)
  (:rest 1:36.5          0)
  (:work 0:20.4        100)
  (:rest 1:34.3          0)
  (:work 0:20.5        100)
  (:rest 1:33.9          0)
  (:work 0:20.1        100)
  (:rest 1:40.7          0)
  (:work 0:19.6        100)
  (:rest 1:41.8          0)
  (:work 0:18.6        100)
  (:rest 1:45.0          0)
  (:work 0:16.5        100)
  ;pause 4:02.0
  (:work 6:15.8 #.(* 6 192))
  (:cool 4:37.0 #.(* 4 192)))

(defworkout "2010-02-07" "13.23"
  (:warm 6:21.43 1.00)
  (:work 5:42.57 1.00)
  (:work 5:42.45 1.00)
  (:work 5:33.33 1.00)
  (:work 5:48.16 1.00)
  (:work 5:54.89 1.00)
  (:work 4:59.23 1.00)
  (:work 5:25.08 1.00)
  (:work 5:27.31 1.00)
  (:work 5:36.92 1.00)
  (:work 5:35.16 1.00)
  (:work 5:35.38 1.00)
  (:work 5:34.90 1.00)
  (:work 5:35.98 1.00)
  (:work 5:43.53 1.00)
  (:work 4:58.48 1.00)
  (:cool 3:50.20 .664))

(defworkout "2010-02-08" "16.20"
  (:warm 6:20    1.00) ;5:50.52???
  (:work 5:42.46 1.00)
  (:work 5:40.60 1.00)
  (:work 3:21.49 .598)
  (:work 0:24.30 .085) ;89.0
  ;pause 2:21.79
  (:hill 0:15.69   85) ;86.9
  (:rest 2:50.89    0)
  (:hill 0:14.50   85) ;90.3
  (:rest 2:57.98    0)
  (:hill 0:14.05   85) ;89.4
  (:rest 2:57.97    0)
  (:hill 0:13.70   85) ;81.8
  (:rest 3:12.98    0)
  (:hill 0:13.67   85) ;88.3
  ;pause 2:35.72
  (:work 5:41.44 1.00)
  (:work 5:40.93 1.00)
  (:work 5:39.55 1.00))

(defworkout "2010-02-10" "11.18"
  (:warm 6:14.09 1.00)
  (:warm 5:41.53 1.00)
  (:warm 5:31.77 1.00)
  (:work 5:06.57 1.00)
  (:work 5:01.88 1.00)
  (:work 5:00.17 1.00)
  (:work 5:01.56 1.00)
  (:work 5:00.39 1.00)
  (:work 4:58.55 1.00)
  (:work 4:56.77 1.00)
  (:cool 4:35.89 .797))

(defworkout "2010-02-11" "16.20"
  (:warm 2:45 400)
  (:warm 2:37 400)
  (:warm 2:33 400))

(defworkout "2010-02-12" "15.12"
  (:warm 5:59.7 #.(* 5 192))
  (:warm 5:27.5 #.(* 5 192))
  (:warm 5:28.3 #.(* 5 192))
  (:warm 5:25.2 #.(* 5 192))
  (:warm 5:17.6 #.(* 5 192))
  (:warm 5:19.3 #.(* 5 192))
  (:warm 4:49.2 #.(* 5 192))
  ;pause 4:32.4
  (:warm 0:21.5 100)
  ;pause 3:32.9
  (:work 0:41.2 200)
  (:rest 3:05.0   0)
  (:work 0:41.0 200)
  (:rest 3:03.9   0)
  (:work 0:36.5 200)
  (:rest 3:22.9   0)
  (:work 0:35.8 200)
  ;pause 7:53.4
  (:warm 0:08.9  50)
  (:rest 1:47.3   0)
  (:warm 0:19.3 100)
  ;pause 3:51.7
  (:work 0:58.7 300)
  (:work 0:18.8 100)
  ;pause 5min?
  (:cool 5:23.9 #.(* 5 192)))

(defworkout "2010-02-14" "10.10"
  (:warm 6:17.55 1.00)
  (:work 5:45.78 1.00)
  (:work 5:38.76 1.00)
  (:work 5:40.73 1.00)
  (:work 6:10.47 1.00)
  (:work 5:11.59 1.00)
  (:work 5:25.87 1.00)
  (:work 5:22.95 1.00)
  (:work 5:31.46 1.00)
  (:work 5:36.39 1.00)
  (:work 5:33.74 1.00)
  (:work 5:44.71 1.00)
  (:work 5:39.10 1.00)
  (:work 5:39.33 1.00)
  (:work 5:56.36 1.00)
  (:work 5:12.35 1.00)
  (:work 5:38.67 1.00)
  (:work 5:04.99 .891))

(defworkout "2010-02-15" "16.49"
  (:warm 6:15.47 1.00)
  (:work 5:42.41 1.00)
  (:work 5:35.33 1.00)
  (:work 5:31.62 1.00)
  (:work 5:14.44 1.00)
  (:work 4:39.68 1.00))

(defworkout "2010-02-17" "10.54"
  (:warm 6:14.05 1.00)
  (:work 5:40.36 1.00)
  (:work 5:36.76 1.00)
  (:work 5:31.88 1.00)
  (:work 5:28.84 1.00)
  (:work 5:33.97 1.00)
  (:work 5:16.11 1.00)
  (:work 5:05.61 1.00)
  (:work 4:54.80 1.00)
  (:work 5:29.74 1.00)
  (:work 2:56.93 .528))

(defworkout "2010-02-19" "10.31"
  (:warm 6:14.97 1.00)
  (:work 5:38.84 1.00)
  (:work 5:40.97 1.00)
  (:work 5:32.76 1.00)
  (:work 5:30.44 1.00)
  (:work 5:42.43 1.00)
  (:work 5:28.42 1.00)
  (:work 5:22.47 1.00)
  (:work 5:35.15 1.00)
  (:work 5:37.43 1.00)
  (:work 1:30.22 .267))

(defworkout "2010-02-20" "12.15"
  (:warm 7:12.2 #.(* 5 192))
  (:warm 6:56.7 #.(* 6 192))
  (:warm 3:56.4 #.(* 4 192))
  ;pause
  (:work 0:04.0         30)
  ;pause 10:27.4
  (:work 0:03.8         30)
  ;pause 8:24.9
  (:warm 3:21.6 #.(* 3 192))
  ;pause 1:15.9
  (:work 0:39.7        200)
  (:rest 1:29.7        200)
  (:work 0:41.3        200)
  (:rest 1:32.2        200)
  (:work 0:41.8        200)
  (:rest 1:33.3        200)
  (:work 0:19.2        200)
  (:work 0:15.9        200)
  ;pause 10:26.3
  (:cool 5:23.6 #.(* 5 192))
  (:cool 5:21.1 #.(* 5 192)))

(defworkout "2010-02-22" "16.33"
  (:warm 6:20.35 1.00)
  (:work 5:47.17 1.00)
  (:work 5:38.44 1.00)
  (:work 3:40.01 .669)
  (:work 0:12.55 .045) ;44.4
  ;pause 3:46.52
  (:hill 0:07.63   40) ;38.9
  (:rest 2:53.95    0)
  (:hill 0:07.55   40) ;47.1
  (:rest 2:53.92    0)
  (:hill 0:07.50   40) ;44.3
  (:rest 2:58.58    0)
  (:hill 0:07.52   40) ;38.0
  (:rest 2:58.20    0)
  (:hill 0:08.18   43) ;51.5
  (:rest 3:08.00    0)
  (:hill 0:07.52   40) ;47.7
  ;pause 1:44.80
  (:work 5:34.83 1.00)
  (:work 5:16.48 1.00)
  (:work 5:27.46 1.00))

(defworkout "2010-02-24" "11.04"
  (:warm 6:12.54 1.00)
  (:warm 5:38.34 1.00)
  (:warm 5:25.95 1.00)
  (:work 5:02.97 1.00)
  (:work 4:55.56 1.00)
  (:work 4:49.45 1.00)
  (:rest 5:25.41 1.00)
  (:work 4:47.97 1.00)
  (:work 3:04.12 .664)
  (:rest 9:29.07    0)
  (:work 4:33.93 1.00)
  (:cool 5:31.21 1.00)
  (:cool 0:58.24 .182))

(defworkout "2010-02-25" "16.30"
  (:work 3:27 500)
  (:work 3:11 500)
  (:work 1:12 200))

(defworkout "2010-02-26" "16.44"
  (:warm 6:08.60 1.00)
  (:warm 5:37.28 1.00)
  (:warm 5:27.28 1.00)
  ;pause 1:13.86
  (:warm 1:57.02 .400) ;402.4
  ;pause 0:57.48
  (:work 1:33.75 .400) ;395.4
  (:rest 0:58.77    0)
  (:work 1:33.68 .400) ;397.3
  (:rest 1:00.62    0)
  (:work 1:30.80 .400) ;405.7
  (:rest 0:59.15    0)
  (:work 1:29.08 .400) ;398.4
  (:rest 1:01.22    0)
  (:work 1:25.08 .400) ;406.1
  ;pause 2:59.57
  (:cool 5:15.84 1.00)
  (:cool 5:23.54 1.00)
  (:cool 5:31.55 1.00))

(defworkout "2010-02-28" "10.52"
  (:warm 6:19.82 1.00)
  (:work 5:39.16 1.00)
  (:work 5:36.80 1.00)
  (:work 5:36.15 1.00)
  (:work 5:55.99 1.00)
  (:work 5:32.48 1.00)
  (:work 5:20.67 1.00)
  (:work 5:11.61 1.00)
  (:work 5:39.66 1.00)
  (:work 5:28.22 1.00)
  (:work 5:29.08 1.00)
  (:work 5:35.33 1.00)
  (:work 5:37.17 1.00)
  (:work 5:14.53 1.00)
  (:work 5:05.28 1.00)
  (:work 4:55.45 1.00)
  (:work 5:41.94 1.00))

(defworkout "2010-03-01" "17.22"
  (:warm 6:15.24 1.00)
  (:work 5:41.76 1.00)
  (:work 5:31.27 1.00)
  (:work 5:22.12 1.00)
  ;pause 1:53.40
  (:hill 0:07.13   45) ;43.8
  (:rest 2:58.05    0)
  (:hill 0:07.65   45) ;44.7
  (:rest 2:56.80    0)
  (:hill 0:07.57   45) ;55.3
  (:rest 2:58.45    0)
  (:hill 0:07.63   45) ;54.3
  (:rest 4:56.82    0)
  (:hill 0:07.95   45) ;50.1
  (:rest 2:54.50    0)
  (:hill 0:07.83   45) ;51.7
  (:rest 3:00.92    0)
  (:hill 0:07.80   45) ;52.0
  (:rest 3:02.73    0)
  (:hill 0:07.80   45) ;52.9
  ;pause 2:15.35
  (:work 5:59.25 1.00)
  (:work 5:35.17 1.00)
  (:work 5:32.43 1.00))

(defworkout "2010-03-03" "10.45"
  (:warm 6:22.87 1.00 :hr (140 :max 147))
  (:warm 5:36.15 1.00 :hr (155 :max 160))
  (:warm 5:25.64 1.00 :hr (161 :max 165))
  (:work 4:54.26 1.00 :hr (171 :max 174))
  (:work 4:50.95 1.00 :hr (175 :max 177))
  (:work 4:43.94 1.00 :hr (178 :max 182))
  (:rest 5:33.25 1.00 :hr (164 :max 179))
  (:work 4:57.11 1.00 :hr (173 :max 177))
  (:work 4:46.59 1.00 :hr (176 :max 179))
  (:work 4:50.90 1.00 :hr (178 :max 182))
  (:cool 5:30.64 1.00 :hr (170 :max 181))
  (:cool 3:23.47 .599 :hr (164 :max 166)))

(defworkout "2010-03-04" "16.30"
  (:warm 3:27 500)
  (:warm 3:16 500)
  (:warm 3:11 500))

(defworkout "2010-03-05" "16.17"
  (:warm 6:15.0 #.(* 5 192))
  (:warm 5:31.7 #.(* 5 192))
  (:warm 5:23.0 #.(* 5 192))
  (:warm 4:57.6 #.(* 5 192))
  (:warm 5:53.9       1056)
  ;pause 6:17.0
  (:warm 0:08.3         50)
  (:rest 1:35.5          0)
  (:warm 0:07.9         50)
  (:rest 1:47.5          0)
  (:warm 0:07.0         50)
  (:rest 5:07.5          0)
  (:warm 0:17.9        100)
  (:rest 2:12.1          0)
  (:warm 0:18.1        100)
  (:rest 2:05.5          0)
  (:warm 0:16.1        100)
  ;pause
  (:work 0:37.2        200)
  (:rest 4:02.3          0)
  (:work 0:36.2        200)
  (:rest 3:58.0          0)
  (:work 0:35.3        200)
  (:rest 3:55.3          0)
  (:work 0:35.3        200)
  (:rest 3:58.9          0)
  (:work 0:34.5        200)
  (:rest 5:12.7          0)
  (:work 0:31.0        200)
  ;pause 11:32.8
  (:cool 5:10.1 #.(* 5 192))
  (:cool 6:30.7 #.(* 6 192)))

(defworkout "2010-03-07" "11.11"
  (:warm 5:46.76 .900) ;164/182
  (:work 5:46.75 1.00) ;167/180
  (:work 5:34.39 1.00 :hr (155 :max 166))
  (:work 5:46.04 1.00 :hr (155 :max 162))
  (:work 5:37.53 1.00 :hr (158 :max 162))
  (:work 6:25.20 1.00 :hr (166 :max 171))
  (:work 5:20.68 1.00 :hr (155 :max 165))
  (:work 5:46.15 1.00 :hr (158 :max 165))
  (:work 5:37.90 1.00 :hr (156 :max 163))
  (:work 5:46.66 1.00 :hr (157 :max 161))
  (:work 5:41.94 1.00 :hr (159 :max 161))
  (:work 5:46.93 1.00 :hr (159 :max 161))
  (:work 5:40.53 1.00 :hr (159 :max 163))
  (:work 5:37.60 1.00 :hr (159 :max 162))
  (:work 5:38.17 1.00 :hr (159 :max 162))
  (:work 5:49.47 1.00 :hr (161 :max 167))
  (:work 5:34.29 1.00 :hr (156 :max 169))
  (:work 2:38.33 .429 :hr (158 :max 161)))

(defworkout "2010-03-13" "12.25"
  (:warm 6:45.8 960)
  (:work 5:59.4 960))

(defworkout "2010-03-16" "16.00"
  (:warm 6:37.49 1.00)
  (:warm 6:16.12 1.00)
  (:work 5:51.69 1.00)
  (:work 5:47.75 1.00)
  (:work 5:47.51 1.00)
  (:work 5:37.38 1.00))

(defworkout "2010-03-26" "16.37"
  (:warm 6:33.38 1.00)
  (:warm 6:07.56 1.00)
  (:work 5:50.32 1.00)
  (:work 5:39.96 1.00))

(defworkout "2010-03-29" "14.54"
  (:warm 6:29.97 1.00)
  (:warm 6:01.77 1.00)
  (:work 5:45.16 1.00)
  (:work 5:44.81 1.00)
  (:work 5:41.08 1.00))

(defworkout "2010-03-31" "16.35"
  (:warm 6:14.82 1.00)
  (:work 5:47.06 1.00)
  (:work 5:34.27 1.00)
  (:work 5:38.30 1.00)
  (:work 5:37.33 1.00)
  (:work 5:24.93 1.00))

(defworkout "2010-04-02" "9.35"
  (:warm 6:39.27 1.00)
  (:work 5:52.13 1.00)
  (:work 5:41.36 1.00)
  (:work 5:34.17 1.00)
  (:work 5:24.14 1.00)
  (:work 5:06.59 1.00)
  (:work 5:29.48 1.00))

(defworkout "2010-04-05" "15.33"
  (:warm 6:26.01 1.00)
  (:work 5:46.39 1.00)
  (:work 5:35.98 1.00)
  (:work 5:35.70 1.00)
  (:work 5:28.01 1.00)
  (:work 5:18.17 1.00)
  (:work 5:05.67 1.00)
  (:work 5:27.55 1.00))

(defworkout "2010-04-08" "16.14"
  (:warm 6:21.60 1.00)
  (:work 5:46.25 1.00)
  (:work 5:34.83 1.00)
  (:work 5:33.92 1.00)
  (:work 5:29.97 1.00)
  (:work 5:28.42 1.00)
  (:work 1:18.20 .241)
  (:work 4:38.60 .884))

(defworkout "2010-04-10" "16.02"
  (:warm 6:28.67 1.00)
  (:work 5:43.90 1.00)
  (:work 5:33.24 1.00)
  (:work 5:38.04 1.00)
  (:work 5:31.23 1.00)
  (:work 5:33.07 1.00)
  (:work 5:26.87 1.00)
  (:work 5:26.51 1.00)
  (:work 4:55.50 1.00)
  (:work 1:41.96 .288))

(defworkout "2010-04-12" "16.18"
  (:warm 6:09.65 1.00)
  (:work 5:40.07 1.00)
  (:work 5:21.35 1.00)
  (:work 5:22.57 1.00)
  (:work 5:25.70 1.00)
  (:work 2:33.53 .461)
  ;pause 0:47.35
  (:work 0:21.85  100) ;101.2
  ;pause 0:56.40
  (:work 0:21.15  100) ;96.7
  ;pause 0:48.27
  (:work 3:38.11 .650))

(defworkout "2010-04-14" "16.08"
  (:warm 6:17.91 1.00)
  (:work 5:36.71 1.00)
  (:work 5:28.37 1.00)
  (:work 5:19.01 1.00)
  (:work 5:08.29 1.00)
  (:work 4:24.52 1.00))

(defworkout "2010-04-16" "17.54"
  (:warm 6:11.82 1.00)
  (:work 5:42.18 1.00)
  (:work 5:30.84 1.00)
  (:work 5:24.44 1.00)
  (:work 5:21.14 1.00)
  (:work 5:19.73 1.00)
  (:work 4:56.66 1.00)
  (:work 5:18.49 1.00))

(defworkout "2010-04-17" "12.11"
  (:warm 13:21.5 1920))

(defworkout "2010-04-19" "16.26"
  (:warm 6:26.22 1.00 :hr (136 :max 147))
  (:work 5:39.76 1.00 :hr (153 :max 156))
  (:work 5:38.83 1.00 :hr (154 :max 156))
  (:work 5:30.35 1.00 :hr (158 :max 167))
  (:work 5:40.22 1.00 :hr (155 :max 158))
  (:work 5:40.03 1.00 :hr (157 :max 165))
  (:work 5:40.65 1.00 :hr (158 :max 161))
  (:work 0:33.86 .100 :hr (162 :max 163)))

(defworkout "2010-04-21" "17.40"
  (:warm 6:00.34 1.00)
  (:work 5:47.46 1.00)
  (:work 5:41.90 1.00)
  (:work 5:43.45 1.00)
  (:work 5:34.55 1.00)
  (:work 5:36.58 1.00)
  (:work 5:29.81 1.00)
  (:work 1:19.56 .216))

(defworkout "2010-04-23" "15.42"
  (:warm 6:24.30 1.00)
  (:work 1:50.19 .315)
  ;pause 4:01.81
  (:work 5:43.10 1.00)
  (:work 5:33.42 1.00)
  (:work 5:31.17 1.00)
  (:work 5:21.79 1.00)
  (:work 5:20.68 1.00)
  (:work 5:13.28 1.00)
  (:work 5:12.41 1.00))

(defworkout "2010-04-26" "16.23"
  (:warm 6:22.47 1.00)
  (:work 1:22.37 .234)
  ;pause 7:36.35
  (:work 5:39.01 1.00)
  (:work 5:34.70 1.00)
  (:work 5:27.69 1.00)
  (:work 5:15.72 1.00)
  ;pause 1:46.43
  (:work 0:17.93  100)
  (:rest 2:48.10    0)
  (:work 0:16.30  100)
  (:rest 2:58.57    0)
  (:work 0:14.70  100)
  ;pause 2:54.08
  (:work 2:38.00 .458))

(defworkout "2010-04-28" "16.53"
  (:warm 6:18.7 1000)
  (:work 5:35.5 1000))

(defworkout "2010-04-28" "18.00"
  (:work 0:13.7 100))

(defworkout "2010-05-01" "15.31"
  (:warm 6:21.29 1.00)
  (:work 5:46.75 1.00)
  (:work 5:33.57 1.00)
  (:work 5:25.53 1.00)
  (:work 3:50.78 .768)
  ;pause 5;58.17
  (:work 4:21.45 .785)
  ;pause 1:35
  (:work 0:08.80   50)
  ;pause 1:24.42
  (:work 0:07.78   50)
  ;pause 2:10.02
  (:work 0:17.75  100)
  ;pause 3:29.70
  (:work 0:39.35  200)
  (:rest 0:30.83    0)
  (:work 0:40.42  200)
  (:rest 0:30.18    0)
  (:work 0:39.22  200)
  (:rest 0:30.98    0)
  (:work 0:37.77  200)
  ;pause 2:53.05
  (:work 3:57.90 .716))

(defworkout "2010-05-03" "16.19"
  (:warm 6:18.42 1.00 :hr (139 :max 150))
  (:work 5:43.01 1.00 :hr (154 :max 165))
  (:work 5:34.76 1.00 :hr (157 :max 160))
  (:work 5:38.32 1.00 :hr (159 :max 163))
  (:work 5:35.55 1.00 :hr (163 :max 165))
  (:work 4:53.97 1.00 :hr (171 :max 176))
  (:work 5:40.47 1.00 :hr (168 :max 177))
  ;pause 2:29.19      :hr (143 :max 164 :min 130)
  (:work 0:16.23  100 :hr (154 :max 165))
  (:rest 3:09.02    0 :hr (143 :max 173 :min 123))
  (:work 0:15.35  100 :hr (154 :max 168))
  ;pause 1:50.26      :hr (155 :max 173 :min 137)
  (:work 2:28.87 .444 :hr (159 :max 173)))

(defworkout "2010-05-05" "16.29"
  (:warm 6:20.2 1000)
  (:work 5:19.0 1000)
  ;pause 4:32.8
  (:work 0:17.5  100)
  (:rest 2:13.2    0)
  (:work 0:15.2  100)
  ;17:13
  (:work 0:17.3  100)
  (:rest 1:56.0    0)
  (:work 0:15.5  100)
  ;pause
  (:work 0:27.4  200))

(defworkout "2010-05-05" "18.24"
  (:work 3:37.7  800)
  ;pause 1:57.5
  (:work 0:40.6  200)
  ;pause 3:10.0
  (:work 0:17.7  100)
  ;pause 4:43.5
  (:work 0:18.9  100)
  ;18.49
  (:work 0:39.2  200)
  (:work 0:39.3  200)
  (:work 0:40.5  200)
  (:work 0:37.6  200)
  ;19:00
  (:work 6:45.1 1200))

(defworkout "2010-05-07" "16.05"
  (:warm 6:22.45 1.00)
  (:work 5:42.80 1.00)
  (:work 5:32.76 1.00)
  (:work 5:36.45 1.00)
  (:work 5:20.05 1.00)
  (:work 5:39.41 1.00))

(defworkout "2010-05-08" "12.15"
  (:warm 7:23.7 960)
  (:work 5:30.6 960)
  (:work 0:56.0 192)
  (:work 0:46.8 192)
  ;pause 1:12:18 <- kulstötning
  (:work 0:17.8 100)
  ;pause 3:05.5
  (:work 0:15.7 100)
  ;pause 4:51.0
  (:work 0:31.3 200)
  ;pause 6:51.6
  (:work 0:16.7 100)
  (:work 0:16.3 100)
  (:work 0:15.7 100))
  
(defworkout "2010-05-11" "16.40"
  (:warm 6:40   1000)
  (:warm 5:30   1000)
  ;pause
  (:warm 2:23.5  400)
  (:warm 4:00.3  800)
  (:warm 2:12.5  400)
  ;pause 1:50.0
  (:warm 0:17.9  100)
  ;pause 1:29.5
  (:warm 0:16.0  100)
  ;pause
  (:warm 0:15.0  100)
  ;pause
  (:work 1:01.5  400)) ;HRmax 184

(defworkout "2010-05-18" "16.35"
  (:warm 6:18.0 1000)
  (:warm 5:16.0 1000)
  ;; 17:48
  (:warm 5:31.8  800)
  (:warm 7:52.7 1200)
  ;pause 8:53.4
  (:warm 0:21.3  100)
  ;pause 3:36.5
  (:warm 0:22.0  100)
  ;pause 2:14.1
  (:warm 0:22.1  100)
  ;; 18:40
  (:work 1:06.0  300 :hr (160 :max 177))
  (:work 1:29.0  400 :hr (182 :max 186))
  (:work 1:29.4  400 :hr (185 :max 187))
  (:work 1:04.6  300 :hr (187 :max 189))
  (:work 0:15.9  100 :hr (189 :max 189)))

(defworkout "2010-05-20" "11.22"
  (:warm 6:36.26 1.00)
  (:work 5:41.08 1.00)
  (:work 5:37.20 1.00)
  (:work 5:32.25 1.00)
  (:work 5:21.03 1.00)
  (:work 5:11.70 1.00)
  (:work 4:51.53 1.00))

(defworkout "2010-05-23" "8.45"
  (:warm 6:19.75 1.00)
  (:work 5:42.37 1.00)
  (:work 5:30.45 1.00)
  (:work 5:27.38 1.00)
  (:work 5:11.84 1.00)
  (:work 3:05.83 .553)
  ;pause 2:12.37
  (:work 0:08.27   50)
  (:rest 2:00.71    0)
  (:work 0:07.05   50)
  (:rest 2:58.82    0)
  (:work 0:06.83   50)
  (:rest 2:42.07    0)
  (:work 0:06.88   50)
  (:rest 3:29.95    0)
  (:work 0:06.35   50)
  ;pause 5:01.76
  (:work 3:14.99 .565))

(defworkout "2010-05-25" "9.40"
  (:warm 1:20    0.2)
  (:warm 6:14.96 1.00)
  (:work 5:47.36 1.00)
  (:work 5:24.64 1.00)
  (:work 5:28.61 1.00)
  (:work 5:17.13 1.00)
  (:work 5:11.29 1.00)
  (:work 4:47.78 1.00)
  (:work 1:11.87 .224)
  ;pause 2:31.75
  (:work 0:07.57   50) ;52
  (:rest 2:34.43    0)
  (:work 0:06.80   50) ;53
  (:rest 2:49.55    0)
  (:work 0:06.59   50));43

(defworkout "2010-06-01" "17.30"
  (:warm 2:56.0 .453)
  (:warm 2:47.2 .453)
  (:warm 2:42.2 .453)
  (:warm 2:23.1 .453)
  ;pause
  (:warm 0:16.6  100)
  ;pause 2:43.7
  (:warm 0:14.2  100)
  ;pause
  (:work 0:13.22 100))

(defworkout "2010-06-02" "16.33"
  (:warm 4:57.5 800)
  (:warm 4:14.7 800)
  ;pause
  ;17:46
  (:warm 0:16.6 100)
  ;pause 2:26.5
  (:warm 0:07.7  60)
  ;pause
  (:work 0:26.8 200))

(defworkout "2010-06-05" "13.42"
  (:warm 6:31.56 1.00)
  (:work 5:58.68 1.00)
  (:work 5:23.49 1.00)
  (:work 5:34.34 1.00)
  (:work 5:28.69 1.00)
  (:work 2:29.06 .521))

(defworkout "2010-06-07" "10.32"
  (:warm 6:24.61 1.00)
  (:work 5:43.38 1.00)
  (:work 5:24.87 1.00)
  (:work 5:30.58 1.00)
  (:work 2:00.92 .369)
  ;pause 3:35.46
  (:work 0:07.80 .050) ;53
  (:rest 3:05.22    0)
  (:work 0:07.48 .050) ;56
  (:rest 3:11.25    0)
  (:work 0:07.20 .050) ;50
  (:rest 5:58.70    0)
  (:work 0:14.05 .100) ;99
  (:rest 8:03.15    0)
  (:work 0:13.10 .100) ;97
  ;pause 5:07.55
  (:work 5:50.62 1.00)
  (:work 3:23.09 .604))

(defworkout "2010-06-09" "10.26"
  (:warm 6:14.70 1.00)
  (:warm 5:38.56 1.00)
  (:work 5:17.67 1.00)
  (:work 5:05.51 1.00)
  (:work 4:52.26 1.00)
  (:cool 5:26.01 1.00)
  (:cool 3:28.84 .604))

(defworkout "2010-06-12" "10.32"
  (:warm 6:12.2 1000)
  (:warm 5:36.8 1000)
  (:warm 1:57.3  400)
  ;pause 15:37
  (:warm 0:19.0  100)
  (:rest 2:16.6    0)
  (:warm 0:17.2  100)
  (:rest 2:18.8    0)
  (:warm 0:15.2  100)
  (:rest 2:52.2    0)
  (:warm 0:38.6  200)
  ;pause 8:34
  (:work 0:59.2  300)
  (:rest 1:56.0    0)
  (:work 0:58.5  300)
  (:rest 1:58.0    0)
  (:work 0:55.8  300)
  (:rest 2:00.0    0)
  (:work 0:56.5  300)
  (:rest 2:00.3    0)
  (:work 0:50.9  300)
  ;pause 11:57
  (:cool 4:14.4  800)
  (:cool 4:39.8  800))

(defworkout "2010-06-15" "16.24"
  (:warm 6:26.13 1.00)
  (:work 5:45.48 1.00)
  (:work 5:24.89 1.00)
  (:work 1:25.17 .277)
  (:rest 3:01.52    0)
  (:work 0:07.73   50)
  (:rest 2:04.37    0)
  (:work 0:06.78   50)
  (:rest 2:27.70    0)
  (:work 0:15.65  100)
  (:rest 2:59.52    0)
  (:work 0:13.85  100)
  (:rest 3:07.35    0)
  (:work 1:25.58 .255))

(defworkout "2010-06-17" "18.15"
  (:warm 2:40   .40)
  (:warm 2:45.7 .44)
  (:warm 2:33.4 .44)
  (:warm 2:25.5 .44)
  (:warm 2:12.8 .44)
  ;pause 
  (:warm 0:19   100)
  ;rest
  (:warm 0:17.6 100)
  ;(:rest 0:55.9   0)
  (:warm 0:19.6 100)
  ;pause 
  (:work 0:37.5 200)
  (:work 1:23.2 400)
  (:work 1:17.8 400)
  ;pause 
  (:cool 2:55.9 .46)
  (:cool 2:13.7 .40))

(defworkout "2010-06-20" "11.53"
  (:warm 6:10.44 1.00)
  (:work 5:36.87 1.00)
  (:work 5:32.34 1.00)
  (:work 5:30.53 1.00)
  (:work 5:30.52 1.00)
  (:work 5:19.68 1.00))

(defworkout "2010-06-22" "16.26"
  (:warm 6:15.13 1.00)
  (:work 5:44.13 1.00)
  (:work 5:32.41 1.00)
  (:work 5:33.77 1.00)
  (:work 5:23.52 1.00)
  (:work 3:28.55 .616)
  ;pause 1:47.46
  (:work 0:17.47  100)
  (:rest 1:57.25    0)
  (:work 0:17.02  100)
  (:rest 1:55.91    0)
  (:work 0:16.15  100)
  (:rest 2:00.95    0)
  (:work 0:13.30  100)
  ;pause 2:07.80
  (:work 2:54.57 .512))

(defworkout "2010-06-24" "9.48"
  (:warm 6:19.23 1.00)
  (:work 5:44.92 1.00)
  (:work 5:28.01 1.00)
  (:work 5:31.90 1.00)
  (:work 5:30.63 1.00)
  (:work 5:30.81 1.00)
  (:work 5:24.62 1.00)
  (:work 2:43.93 .500))

(defworkout "2010-06-28" "16.33"
  (:warm 6:07.32 1.00)
  (:work 5:36.73 1.00)
  (:work 5:29.97 1.00)
  (:work 5:09.22 1.00)
  (:work 4:56.43 1.00)
  (:work 5:31.16 1.00)
  (:work 1:14.54 .197))

(defworkout "2010-06-30" "11.06"
  (:warm 6:16.94 1.00)
  (:work 5:45.52 1.00)
  (:work 5:36.12 1.00)
  (:work 5:33.15 1.00)
  (:work 5:30.04 1.00)
  (:work 5:24.28 1.00)
  (:work 4:53.80 1.00)
  (:work 5:28.70 1.00))

(defworkout "2010-07-02" "16.14"
  (:warm 5:58.63  .95)
  (:work 5:08.53  .95)
  (:work 5:36.93 1.00)
  (:work 5:35.02 1.00)
  (:work 5:30.39 1.00)
  (:work 2:14.17 .408)
  ;pause 2:06.17
  (:work 0:18.05  100)
  (:rest 2:02.23    0)
  (:work 0:16.92  100)
  (:rest 2:01.63    0)
  (:work 0:16.62  100)
  (:rest 2:13.18    0)
  (:work 0:15.47  100)
  ;pause 2:08.55
  (:cool 4:14.94 .739))

(defworkout "2010-07-04" "11.01"
  (:warm 6:12.51 1.00)
  (:work 5:42.93 1.00)
  (:work 5:40.64 1.00)
  (:work 5:29.02 1.00)
  (:work 5:17.90 1.00)
  (:work 5:30.16 1.00)
  (:work 5:34.01 1.00)
  (:work 4:08.65 .734))

(defworkout "2010-07-06" "16.02"
  (:warm 6:19.78 1.00)
  (:work 5:37.46 1.00)
  (:work 5:31.67 1.00)
  (:work 5:26.52 1.00)
  (:work 5:02.15 1.00)
  (:work 4:39.74 1.00))

(defworkout "2010-07-08" "10.55"
  (:warm 6:21.00 1.00)
  (:work 5:45.81 1.00)
  (:work 5:34.49 1.00)
  (:work 5:35.70 1.00)
  (:work 5:30.23 1.00)
  (:work 5:19.68 1.00)
  (:work 5:25.59 1.00)
  (:work 5:25.69 1.00)
  (:work 2:41.65 .507))

(defworkout "2010-07-10" "14.01"
  (:warm 6:10.5 1000)
  (:warm 5:29.4 1000)
  (:warm 5:16.8 1000)
  ;pause 13:48.7
  (:warm 0:25.8  100)
  (:rest 7:25.0    0)
  (:work 0:16.4   60) ;4:33/km (2 steps 81, face 124)
  (:rest 2:02.3    0)
  (:work 0:13.1   60) ;3:38/km
  (:rest 1:03.7    0)
  (:work 0:11.7   60) ;3:15/km
  (:rest 8:32.0    0)
  (:work 0:10.9   60) ;3:02/km (step 35, face 78)
  (:rest 3:07.3    0)
  (:work 0:09.9   60) ;2:45/km
  (:rest 4:11.8    0)
  (:work 0:08.7   60) ;2:25/km
  (:rest 5:23.5    0)
  (:work 0:08.1   60));2:15/km (2 steps 122, face 147)

(defworkout "2010-07-12" "10.08"
  (:warm 6:35.70 1.00)
  (:work 5:45.00 1.00)
  (:work 5:36.98 1.00)
  (:work 5:32.71 1.00)
  (:work 5:29.55 1.00)
  (:work 5:27.52 1.00)
  (:work 5:10.15 1.00))

(defworkout "2010-07-14" "15.24"
  (:warm 6:10.7 1000)
  (:warm 5:39.6 1000)
  (:warm 5:24.2 1000)
  ;pause 11:37.1
  (:warm 0:10.0   60)
  ;pause 1:45.7
  (:warm 0:09.0   60)
  ;pause 4:43.3
  (:warm 0:23.1  100) ;3:51/km, 180 steps/min
  ;pause 0:47.3
  (:warm 0:25.8  100) ;4:18/km, 180 steps/min
  ;pause 3:49.8
  (:work 1:48.5  400) ;4:31/km, 179 steps/min
  (:rest 0:59.1    0)
  (:work 1:47.1  400)
  (:rest 0:59.7    0)
  (:work 1:44.0  400)
  (:rest 0:59.6    0)
  (:work 1:43.3  400)
  (:rest 1:00.2    0)
  (:work 1:39.2  400)
  (:rest 1:00.3    0)
  (:work 1:36.4  400)
  (:rest 1:00.8    0)
  (:work 1:34.9  400)
  (:rest 0:59.1    0)
  (:work 1:28.9  400));3:42/km, 185 steps/min

(defworkout "2010-07-17" "8.25"
  (:warm 6:18.22 1.00)
  (:work 5:45.96 1.00)
  (:work 5:36.12 1.00)
  (:work 5:34.64 1.00)
  (:work 5:28.11 1.00)
  (:work 5:29.31 1.00)
  (:work 5:29.65 1.00)
  (:work 5:16.98 1.00)
  (:work 5:03.71 1.00))

(defworkout "2010-07-19" "10.03"
  (:warm 6:28.87 1.00)
  (:warm 5:38.18 1.00)
  (:warm 5:26.23 1.00)
  (:work 4:56.98 1.00)
  (:work 4:50.40 1.00)
  (:work 4:44.41 1.00)
  (:cool 5:01.08 .920))

(defworkout "2010-07-25" "8.33"
  (:warm 6:18.88 1.00)
  (:work 5:37.08 1.00)
  (:work 5:22.24 1.00)
  (:work 5:20.43 1.00)
  (:work 5:20.58 1.00)
  (:work 5:06.52 1.00))

(defworkout "2010-07-27" "10.06"
  (:warm 6:15.62 1.00)
  (:work 5:36.32 1.00)
  (:work 5:28.13 1.00)
  (:work 5:26.92 1.00)
  (:work 5:24.11 1.00)
  (:work 5:31.56 1.00)
  (:work 5:17.04 1.00)
  (:work 5:08.75 1.00)
  (:rest 3:46.94    0)
  (:work 0:07.58   50)
  (:rest 3:25.77    0)
  (:work 0:07.08   50))

(defworkout "2010-07-29" "15.13"
  (:warm  6:12.3 1000)
  (:warm  5:33.0 1000)
  (:warm  5:10.9 1000)
  (:rest 13:27.1    0)
  (:work  0:06.3   40)
  (:rest  3:27.8    0)
  (:work  0:05.5   40)
  (:rest  6:03.0    0)
  (:work  0:05.3   40)
  (:rest  6:12.9    0)
  (:work  0:05.1   40)
  (:rest  7:32.5    0)
  (:work  0:27.8  200)
  (:rest 11:56.0    0)
  (:work  0:27.3  200)
  ;pause
  (:cool  0:36    100))

(defworkout "2010-07-31" "8.38"
  (:warm  6:29.40 1.00)
  (:work  5:41.48 1.00)
  (:work  5:27.31 1.00)
  (:work  5:23.23 1.00)
  (:work  5:19.53 1.00)
  (:work  5:20.03 1.00)
  (:work  5:18.45 1.00)
  (:work  5:18.60 1.00)
  (:work  5:21.30 1.00))

(defworkout "2010-08-02" "15.37"
  (:warm 6:08.17 1.00)
  (:work 5:36.23 1.00)
  (:work 5:30.21 1.00)
  (:work 5:23.70 1.00)
  (:work 2:02.51 .372)
  ;pause 4:37.97
  (:work 0:19.88  100)
  (:rest 1:04.70    0)
  (:work 0:18.55  100)
  (:rest 1:04.30    0)
  (:work 0:17.90  100)
  (:rest 1:05.80    0)
  (:work 0:17.00  100)
  (:rest 1:04.37    0)
  (:work 0:16.28  100)
  ;pause 2:45.37
  (:work 4:06.55 .734))

(defworkout "2010-08-04" "16.04"
  (:warm  6:29.5 1000)
  (:warm  5:40.2 1000)
  (:warm  5:21.5 1000)
  ;pause  6:34.7
  (:work  0:21.0  100)
  (:rest  1:40.2    0)
  (:work  0:18.9  100)
  (:rest  2:59.7    0)
  (:work  0:35.6  200)
  ;pause ~9:00
  (:work  1:11.0  400)
  (:work  0:18.2  100)
  (:work  0:18.3  100)
  ;pause 17:00
  (:work  0:08.5   60) ;?
  (:work  0:17.7  100)
  (:rest 15:58.8    0)
  (:work  0:36.6  250))

(defworkout "2010-08-06" "10.08"
  (:warm 6:01.32 1.00)
  (:work 5:36.73 1.00)
  (:work 5:25.53 1.00)
  (:work 5:16.75 1.00)
  (:work 5:17.04 1.00)
  (:work 5:14.77 1.00)
  (:work 4:37.46 1.00))

(defworkout "2010-08-08" "9.30"
  (:warm 6:08.25 1.00)
  (:work 5:49.08 1.00)
  (:work 5:39.43 1.00)
  (:work 5:36.70 1.00)
  (:work 5:33.48 1.00)
  (:work 5:40.50 1.00)
  (:work 5:25.74 1.00)
  (:work 5:42.77 1.00)
  (:work 5:37.95 1.00)
  (:work 5:12.87 1.00))

(defworkout "2010-08-10" "10.07"
  (:warm 5:56.65 1.00)
  (:work 5:25.60 1.00)
  (:work 5:19.41 1.00)
  (:work 5:17.01 1.00)
  (:work 5:12.67 1.00)
  (:work 5:20.47 1.00)
  (:work 5:29.08 1.00)
  ;pause 6:00.28
  (:work 0:08.30   50)
  (:rest 2:01.55    0)
  (:work 0:08.05   50)
  (:rest 2:01.47    0)
  (:work 0:07.78   50)
  (:rest 2:04.35    0)
  (:work 0:07.55   50))

(defworkout "2010-08-12" "15.48"
  (:warm  6:01.4 1000 :hr (144 :max 158))
  (:warm  5:28.4 1000 :hr (163 :max 168))
  (:warm  5:14.4 1000 :hr (167 :max 169))
  ;pause 12:01.0      :hr 148/170
  (:warm  0:19.5  100 :hr (148 :max 159))
  (:rest  2:22.2    0 :hr (138 :max 168))
  (:warm  0:18.5  100 :hr (139 :max 159))
  ;pause 13:25.3           123/168
  (:work  1:36.1  400 :hr (169 :max 180))
  (:work  1:36.5  400 :hr (182 :max 185))
  (:rest  3:01.8    0 :hr (142 :max 185 :end 129))
  (:work  1:39.0  400 :hr (167 :max 181))
  (:work  1:34.1  400 :hr (184 :max 186))
  (:rest  2:59.7    0 :hr (149 :max 185 :end 129))
  (:work  1:37.5  400 :hr (166 :max 184))
  (:work  1:35.0  400 :hr (184 :max 186))
  (:rest  3:00.8    0 :hr (153 :max 185 :end 136))
  (:work  1:41.0  400 :hr (171 :max 183))
  (:work  1:09.8  300 :hr (184 :max 185))
  (:work  0:18.3  100 :hr (185 :max 186))
  ;pause 16:00.8           140/186
  (:cool  5:17.1 1000)
  (:cool  5:27.5 1000))

(defworkout "2010-08-15" "11.33"
  (:warm  6:02.3 1000)
  (:warm  5:34.2 1000)
  (:warm  5:25.0 1000)
  ;pause 24:33.8
  (:warm  0:05.9   40)
  (:rest  4:29.7    0)
  (:work  0:05.3   40) ;118 frames (5228-5346)
  (:rest  5:22.0    0)
  (:work  0:05.0   40) ;116 frames (6248-6364)
  (:rest  5:09.6    0)
  (:work  0:05.3   40) ;119 frames (5532-5651)
  (:rest  5:00      0)
  (:work  0:05.1   40) ;114 frames
  ;pause 20:00
  (:work  0:36.2  250) ;4340 frames (22-4362)
  ;pause 20:00
  (:work  0:36.8  250));4410 frames (195-4605)

(defworkout "2010-08-18" "16.34"
  (:warm  6:11.4  1000)
  (:warm  5:37.9  1000)
  ;pause  6:41.2
  (:warm  0:12.6    60)
  ;pause  0:35.0
  (:warm  0:09.8    60)
  ;pause
  (:work  0:13.06  100))

(defworkout "2010-08-20" "8.12"
  (:warm  5:56.47 1.00)
  (:warm  5:32.68 1.00)
  (:warm  5:23.06 1.00)
  (:work  4:52.63 1.00)
  (:work  4:47.69 1.00)
  (:work  4:38.02 1.00))

(defworkout "2010-08-22" "12.26"
  (:warm  6:06.0 1000)
  (:warm  5:23.3 1000)
  (:warm  5:20.6 1000)
  ;pause 34:11.4
  (:warm  0:07.0   40)
  (:rest  1:39.5    0)
  (:warm  0:06.3   40)
  (:rest  3:11.7    0)
  (:warm  0:18.2  100)
  (:rest  4:23.5    0)
  (:warm  0:16.8  100)
  ;pause  8:01.1
  (:work  0:53.5  300) ;282.6@50.4, 317.4@56.6
  (:work  0:18.4  100)
  (:work  0:35.0  200)
  ;pause 13:18.7
  (:cool  5:27.8 1000))

(defworkout "2010-08-25" "17.39"
  (:warm  4:54.5 800)
  (:warm  4:18.5 800)
  ;pause  8:16.7
  (:warm  0:11.1  60)
  (:rest  1:26.4   0)
  (:warm  0:10.6  60)
  ;pause
  (:warm 12:00   2.0)
  ;pause
  (:warm  0:19.9 100)
  ;pause
  (:work  0:77.80  400)
  (:work  0:73.64  400)) ;2:31.44 (2:31.2m)


(defworkout "2010-08-28" "9.58"
  (:warm  5:53.7 1000)
  (:warm  5:25.7 1000)
  ;pause  8:34.4
  (:warm  0:18.7  100)
  ;pause 12:47.3
  (:warm  0:15.0  100)
  ;pause  8:57.2
  (:warm  0:13.2  100)
  ;pause 16:00
  (:work  0:46.0  300)
  ;pause 10:00
  (:cool  5:25.6 1000))

(defworkout "2010-08-30" "15.25"
  (:warm  5:43.7 1000)
  (:warm  5:19.4 1000)
  ;pause  8:32.3
  (:warm  0:15.9  100)
  ;pause 14:32.3
  (:work  0:14.3  100)
  (:rest 11:01.8    0)
  (:work  0:14.3  100)
  (:rest  9:08.5    0)
  (:work  0:14.2  100))

(defworkout "2010-09-01" "16.19"
  (:warm  4:50.4  800)
  (:warm  2:08.3  400)
  ;pause 10:18.4
  (:warm  0:10     60)
  ;pause 15:28.0
  (:warm  0:05.5   40)
  ;pause  4:18.9
  (:warm  0:15.3  100)
  ;pause
  (:work  0:59.74 400))

(defworkout "2010-09-08" "9.25"
  (:warm 6:10.58 1.00)
  (:work 5:45.04 1.00)
  (:work 5:38.22 1.00)
  (:work 5:31.19 1.00)
  (:work 5:29.35 1.00)
  (:work 5:39.57 1.00)
  (:work 5:42.85 1.00)
  (:work 5:45.39 1.00)
  (:work 5:44.10 1.00))

(defworkout "2010-09-16" "10.05"
  (:warm 6:23.93 1.00)
  (:work 5:47.53 1.00)
  (:work 5:34.99 1.00)
  (:work 5:36.58 1.00)
  (:work 5:30.25 1.00)
  (:work 5:27.37 1.00)
  (:work 5:33.81 1.00)
  (:work 5:17.39 1.00)
  (:work 5:05.97 1.00))

(defworkout "2010-09-19" "12.39"
  (:warm 6:11.35 1.00)
  (:work 5:43.95 1.00)
  (:work 5:44.23 1.00)
  (:work 5:35.93 1.00)
  (:work 5:39.35 1.00)
  (:work 5:30.20 1.00)
  (:work 5:34.69 1.00)
  (:work 5:34.75 1.00)
  (:work 5:30.75 1.00)
  (:work 5:38.95 1.00))

(defworkout "2010-09-21" "9.50"
  (:warm 5:55.83 1.00)
  (:work 5:36.81 1.00)
  (:work 5:18.20 1.00)
  (:work 5:18.63 1.00)
  (:work 5:08.83 1.00))

(defworkout "2010-09-27" "11.21"
  (:warm 6:12.15 1.00)
  (:work 5:34.19 1.00)
  (:work 5:33.32 1.00)
  (:work 5:23.72 1.00)
  (:work 5:21.37 1.00)
  (:work 5:21.42 1.00))

(defworkout "2010-09-29" "11.46"
  (:warm 5:57.79 1.00)
  (:work 5:33.42 1.00)
  (:work 5:24.44 1.00)
  (:work 5:28.43 1.00)
  (:work 5:22.58 1.00)
  (:work 5:07.92 1.00)
  (:work 4:54.80 1.00))

(defworkout "2010-10-02" "9.35"
  (:warm 6:14.63 1.00)
  (:work 5:33.28 1.00)
  (:work 5:23.18 1.00)
  (:work 5:23.50 1.00)
  (:work 5:22.26 1.00)
  (:work 5:20.36 1.00)
  (:work 5:22.10 1.00)
  (:work 5:25.61 1.00)
  (:work 5:27.09 1.00)
  (:work 5:22.80 1.00)
  (:work 5:19.08 1.00))

(defworkout "2010-10-04" "12.10"
  (:warm 6:02.12 1.00)
  (:work 5:36.10 1.00)
  (:work 5:26.67 1.00)
  (:work 5:23.78 1.00)
  (:work 5:25.06 1.00)
  (:work 3:10.24 .584)
  ;pause 2:19.17
  (:work 0:09.10   50) ;61
  (:rest 1:29.30    0)
  (:work 0:08.83   50) ;55
  (:rest 1:35.55    0)
  (:work 0:07.92   50));56

(defworkout "2010-10-06" "15.10"
  (:warm 5:52.53 1.00)
  (:warm 5:23.44 1.00)
  (:warm 5:12.91 1.00)
  (:work 5:00.20 1.00)
  (:work 4:56.55 1.00)
  (:work 4:47.88 1.00)
  (:cool 5:12.18 .95))

(defworkout "2010-10-08" "10.12"
  (:warm 6:00.71 1.00)
  (:work 5:34.96 1.00)
  (:work 5:17.88 1.00)
  (:work 5:26.10 1.00)
  (:work 5:22.04 1.00)
  (:work 5:19.35 1.00)
  (:work 5:13.06 .95))

(defworkout "2010-10-10" "10.13"
  (:warm 6:06.29 1.00)
  (:work 5:34.01 1.00)
  (:work 5:30.17 1.00)
  (:work 5:30.17 1.00)
  (:work 5:26.11 1.00)
  (:work 5:26.41 1.00)
  (:work 5:27.56 1.00)
  (:work 5:27.73 1.00)
  (:work 5:30.21 1.00)
  (:work 5:25.47 1.00)
  (:work 4:58.22 1.00)
  (:cool 1:24.72 .199))

(defworkout "2010-10-12" "10.35"
  (:warm 5:58.07 1.00)
  (:work 5:32.04 1.00)
  (:work 5:19.46 1.00)
  (:work 5:27.76 1.00)
  (:work 5:20.47 1.00)
  (:work 5:25.85 1.00)
  (:work 5:23.68 1.00)
  (:rest 2:01.84    0)
  (:work 0:07.90   50) ;45.6
  (:rest 2:00.20    0)
  (:work 0:07.40   50) ;50.9
  (:rest 2:02.97    0)
  (:work 0:07.35   50) ;44.6
  (:rest 2:03.73    0)
  (:work 0:06.67   50));37.9

(defworkout "2010-10-14" "10.42"
  (:warm 5:43.80 1.00)
  (:warm 5:21.37 1.00)
  (:work 4:49.53 1.00)
  (:work 4:53.13 1.00)
  (:work 4:49.26 1.00)
  (:work 4:43.56 1.00)
  (:cool 5:33.21 1.00))

(defworkout "2010-10-16" "10.30"
  (:warm 6:04.82 1.00)
  (:work 5:35.89 1.00)
  (:work 5:30.92 1.00)
  (:work 5:25.92 1.00)
  (:work 5:24.21 1.00)
  (:work 5:25.16 1.00)
  (:work 5:27.13 1.00)
  (:work 5:29.20 1.00)
  (:work 5:25.35 1.00)
  (:work 5:10.06 1.00)
  (:work 5:05.76 1.00)
  (:cool 1:55.05 .328))

(defworkout "2010-10-18" "11.40"
  (:warm 6:01.01 1.00)
  (:work 5:38.40 1.00)
  (:work 5:29.50 1.00)
  (:work 5:19.51 1.00)
  (:work 5:14.67 1.00)
  (:work 5:15.91 1.00)
  (:work 5:07.36 1.00))

(defworkout "2010-10-20" "11.35"
  (:warm 6:01.17 1.00)
  (:warm 5:32.43 1.00)
  (:warm 5:18.14 1.00)
  (:work 5:11.12 1.00)
  (:work 5:08.12 1.00)
  (:work 5:09.08 1.00)
  (:work 5:02.74 1.00)
  (:cool 5:26.29 1.00))

(defworkout "2010-10-22" "15.52"
  (:warm 6:01.30 1.00)
  (:work 5:29.28 1.00)
  (:work 5:25.69 1.00)
  (:work 5:17.80 1.00)
  (:work 5:12.79 1.00)
  (:work 5:14.02 1.00)
  (:work 4:52.44 1.00))

(defworkout "2010-10-24" "10.30"
  (:warm 6:03.99 1.00)
  (:work 5:34.62 1.00)
  (:work 5:28.50 1.00)
  (:work 5:31.87 1.00)
  (:work 5:24.22 1.00)
  (:work 5:17.65 1.00)
  (:work 5:22.15 1.00)
  (:work 5:29.63 1.00)
  (:work 5:38.84 1.00)
  (:work 5:32.94 1.00)
  (:work 5:28.54 1.00)
  (:work 5:14.80 1.00)
  (:cool 1:13.97 .209))

(defworkout "2010-10-26" "10.21"
  (:warm 5:47.92 1.00)
  (:warm 5:32.06 1.00)
  (:warm 5:16.03 1.00)
  (:work 5:04.36 1.00)
  (:work 4:54.72 1.00)
  (:work 4:57.64 1.00)
  (:work 4:51.94 1.00)
  (:cool 5:22.34 1.00))

(defworkout "2010-10-28" "10.38"
  (:warm 6:03.23 1.00)
  (:warm 5:34.65 1.00)
  (:warm 5:20.30 1.00)
  (:warm 5:22.18 1.00)
  (:warm 4:16.14 .803)
  (:work 0:54.22  200) ;196
  (:rest 1:13.12  200) ;212
  (:work 0:54.37  200) ;226 58.50
  (:rest 1:15.03  200) ;194 69.33
  (:work 0:53.62  200) ;208
  (:rest 1:15.12  200) ;201
  (:work 0:54.36  200) ;200
  (:rest 1:11.70  200) ;208
  (:work 0:53.97  200) ;210
  (:rest 1:13.33  200) ;195
  (:work 0:51.45  200) ;207
  (:rest 1:08.32  200) ;196
  ;pause 7:42.20
  (:cool 5:31.23 1.00)
  (:cool 5:24.53 1.00))

(defworkout "2010-11-01" "10.56"
  (:warm 6:04.65 1.00)
  (:work 5:37.83 1.00)
  (:work 5:24.79 1.00)
  (:work 5:31.09 1.00)
  (:work 5:23.58 1.00)
  (:rest 3:11.53    0)
  (:hill 0:12.47   70)
  (:rest 1:35.45    0)
  (:hill 0:12.33   70)
  (:rest 2:37.03    0)
  (:hill 0:11.57   70)
  (:rest 2:47.57    0)
  (:work 5:24.28 1.00)
  (:work 5:20.94 1.00)
  (:work 2:15.96 .415))

(defworkout "2010-11-03" "9.41"
  (:warm 6:03.37 1.00)
  (:warm 5:36.70 1.00)
  (:warm 5:20.62 1.00)
  (:work 5:08.59 1.00)
  (:work 5:09.10 1.00)
  (:work 4:55.86 1.00)
  (:work 4:54.53 1.00)
  (:cool 5:19.16 1.00)
  (:cool 4:22.66 .792))

(defworkout "2010-11-05" "10.17"
  (:warm 5:13.73 .85)
  (:work 5:34.20 1.00)
  (:work 5:29.39 1.00)
  (:work 5:31.53 1.00)
  (:work 5:19.46 1.00)
  (:work 5:18.57 1.00)
  (:work 5:08.52 1.00)
  (:work 5:30.99 1.00)
  (:cool 4:35.73 .80))

(defworkout "2010-11-06" "12.07"
  (:warm 4:43.32 .711)
  ;pause 0:32.42
  (:hill 1:01.70 .106)
  ;pause 3:00.33
  (:hill 0:27.55 .084)
  (:rest 0:55.55 .102)
  (:hill 0:30.85 .089)
  (:rest 0:58.12 .104)
  (:hill 0:28.72 .083)
  (:rest 0:54.93 .105)
  (:hill 0:28.43 .090)
  (:rest 0:52.97 .097)
  (:hill 0:26.20 .086)
  (:rest 0:49.90 .101)
  ;pause 3:00.95
  (:hill 0:27.33 .085)
  (:rest 0:51.92 .103)
  (:hill 0:28.03 .089)
  (:rest 0:53.95 .101)
  (:hill 0:26.75 .096)
  (:rest 0:51.07 .102)
  (:hill 0:28.95 .087)
  (:rest 0:51.13 .101)
  (:hill 0:22.57 .091)
  ;pause
  (:cool 3:00    .500)
  (:cool 6:27.48 1.00)
  (:cool 3:29.52 .560))
  
(defworkout "2010-11-08" "11.54"
  (:warm 5:56.35 1.00)
  (:work 5:32.79 1.00)
  (:work 5:28.91 1.00)
  (:work 5:20.46 1.00)
  (:work 5:36.54 1.00)
  (:work 5:10.48 1.00)
  (:work 5:24.86 1.00)
  (:work 5:12.55 1.00)
  (:cool 3:22.00 .602))

(defworkout "2010-11-10" "10.33"
  (:warm 5:52.68 1.00)
  (:warm 5:30.49 1.00)
  (:warm 5:19.59 1.00)
  (:work 5:01.92 1.00)
  (:work 4:59.62 1.00)
  (:work 5:00.02 1.00)
  (:work 4:54.49 1.00)
  (:work 4:53.67 1.00)
  (:cool 5:20.43 1.00))

(defworkout "2010-11-12" "11.22"
  (:warm 6:00.79 1.00)
  (:warm 5:29.90 1.00)
  (:warm 5:21.82 1.00)
  (:warm 5:02.65 1.00)
  ;pause 2:16.91
  (:work 0:51.50  200) ;206
  (:rest 1:08.97  200) ;199
  (:work 0:52.60  200) ;204
  (:rest 1:07.03  200) ;218 72.70
  (:work 0:50.50  200) ;184 46.23
  (:rest 1:10.12  200) ;199
  (:work 0:53.15  200) ;202
  (:rest 1:07.40  200) ;195
  (:work 0:52.63  200) ;213
  (:rest 1:09.10  200) ;194
  (:work 0:51.37  200) ;202
  (:rest 1:11.15  200) ;197
  (:work 0:51.19  200) ;203
  (:rest 1:07.97  200) ;200
  (:work 0:44.43  200) ;205
  ;pause 3:25.47
  (:cool 5:27.27 1.00)
  (:cool 3:18.26 .600))

(defworkout "2010-11-14" "10.18"
  (:warm 5:59.49 1.00)
  (:work 5:33.11 1.00)
  (:work 5:30.02 1.00)
  (:work 5:25.47 1.00)
  (:work 5:24.30 1.00)
  (:work 5:23.00 1.00)
  (:work 5:20.16 1.00)
  (:work 5:27.41 1.00)
  (:work 5:22.75 1.00)
  (:work 5:28.66 1.00)
  (:work 5:08.98 1.00)
  (:work 5:14.35 1.00)
  (:work 4:28.54 .799))

(defworkout "2010-11-16" "10.21"
  (:warm 5:53.39 1.00)
  (:warm 5:26.00 1.00)
  (:warm 5:18.45 1.00)
  (:warm 5:11.17 1.00)
  (:work 4:55.53 1.00)
  (:work 4:54.59 1.00)
  (:work 4:55.61 1.00)
  (:work 4:47.94 1.00)
  (:cool 5:16.06 1.00))

(defworkout "2010-11-18" "10.30"
  (:warm 5:48.43 1.00)
  (:work 5:28.19 1.00)
  (:work 5:18.18 1.00)
  (:work 5:19.63 1.00)
  (:work 5:04.39 1.00)
  (:work 1:10.06 .206)
  ;pause 1:58.82
  (:hill 0:11.18   60) ;69
  (:rest 1:57.00    0)
  (:hill 0:10.35   60) ;68
  (:rest 2:00.27    0)
  (:hill 0:10.42   60) ;63
  (:rest 1:58.69    0)
  (:hill 0:10      60)
  ;pause
  (:work 5:20.83 1.00)
  (:work 5:18.06 1.00)
  (:work 2:41.16 .516))

(defworkout "2010-11-20" "10.11"
  (:warm 5:55.82 1.00)
  (:work 5:38.96 1.00)
  (:work 5:25.96 1.00)
  (:work 5:24.30 1.00)
  (:work 5:18.10 1.00)
  (:work 5:14.44 1.00)
  (:work 5:35.08 1.00)
  (:work 5:24.79 1.00)
  (:work 5:22.90 1.00)
  (:work 5:27.92 1.00)
  (:work 5:26.13 1.00)
  (:work 5:22.05 1.00)
  (:work 5:09.01 1.00)
  (:cool 1:19.81 .209))

(defworkout "2010-11-22" "10.48"
  (:warm 5:48.28 1.00)
  (:work 5:27.31 1.00)
  (:work 5:23.37 1.00)
  (:work 5:22.03 1.00)
  (:work 5:32.87 1.00)
  (:work 5:16.57 1.00)
  (:work 4:17.22 .799)
  (:rest 1:57.00    0)
  (:work 0:19.07  100) ;107
  (:rest 2:01.40    0)
  (:work 0:17.72  100) ;113
  (:rest 1:56.40    0)
  (:work 0:17.03  100) ;102
  (:rest 2:00.90    0)
  (:work 5:28.45 1.00)
  (:work 2:11.27 .412))

(defworkout "2010-11-24" "10.12"
  (:warm 5:46.51 1.00)
  (:warm 5:34.46 1.00)
  (:warm 5:23.22 1.00)
  (:work 5:08.87 1.00)
  (:work 5:06.67 1.00)
  (:work 5:15.39 1.00)
  (:work 5:00.78 1.00)
  (:work 5:00.77 1.00)
  (:work 4:52.02 1.00)
  (:cool 2:04.36 .371))

(defworkout "2010-11-26" "16.21"
  (:warm 6:03.1 960 :hr (142 :max 155))
  (:warm 5:22.7 960 :hr (159 :max 163))
  (:warm 5:16.8 960 :hr (164 :max 168))
  ;pause 2:16.2   0 :hr (135 :max 163 :min 126)
  (:warm 0:15.7  80 :hr (131 :max 155))
  ;pause 2:12.8   0 :hr (128 :max 160 :min 116)
  (:warm 0:15.6  80 :hr (130 :max 149))
  ;pause 6:00            123      159      116
  (:work 0:50.4 200 :hr (161 :max 168))
  (:rest 1:07.7 200 :hr (169 :max 171 :min 170))
  (:work 0:51.0 200 :hr (173 :max 176))
  (:rest 1:08.8 200 :hr (174 :max 177 :min 172))
  (:work 0:50.3 200 :hr (175 :max 179))
  (:rest 1:06.6 200 :hr (176 :max 180 :min 173))
  (:work 0:48.2 200 :hr (179 :max 183))
  (:rest 1:09.2 200 :hr (179 :max 183 :min 173))
  (:work 0:46.3 200 :hr (180 :max 186))
 ;(:rest 2:57.8 200 :hr (164 :max 186 :min 114))
  (:rest 1:10   200 :hr (179 :max 186))
  ;pause 1:47.8 200 :hr (             :min 114))
  (:work 0:44.2 200 :hr (156 :max 175))
  (:rest 1:06.7 200 :hr (176 :max 178 :min 173))
  (:work 0:48.6 200 :hr (178 :max 181))
  (:rest 1:08.8 200 :hr (179 :max 180 :min 176))
  (:work 0:46.2 200 :hr (181 :max 185))
  (:rest 1:07.6 200 :hr (183 :max 186 :min 178))
  (:work 0:46.8 200 :hr (182 :max 186))
  (:rest 1:08.4 200 :hr (182 :max 187 :min 177))
  (:work 0:42.3 200 :hr (184 :max 187))
  (:rest 1:06.1 200 :hr (184 :max 188 :min 178))
  ;pause 5:21.5          134      178      127
  (:cool 5:21.6 960 :hr (160 :max 166)))

(defworkout "2010-11-28" "10.14"
  (:warm 6:01.76 1.00)
  (:work 5:35.60 1.00)
  (:work 5:31.64 1.00)
  (:work 5:32.97 1.00)
  (:work 5:26.13 1.00)
  (:work 5:31.46 1.00)
  (:work 5:32.97 1.00)
  (:work 5:32.65 1.00)
  (:work 5:26.18 1.00)
  (:work 5:29.62 1.00)
  (:work 5:26.36 1.00)
  (:work 5:24.43 1.00)
  (:cool 5:48.26 1.00))

(defworkout "2010-11-29" "10.05"
  (:warm 5:48.66 1.00)
  (:work 5:31.65 1.00)
  (:work 5:27.23 1.00)
  (:work 5:29.63 1.00)
  (:work 5:14.15 1.00)
  (:work 5:14.66 1.00)
  (:work 4:47.99 .904))

(defworkout "2010-12-01" "11.00"
  (:work 50 9.4))

(defworkout "2010-12-03" "12.04"
  (:warm 5:46.24 1.00)
  (:warm 5:30.56 1.00)
  (:warm 5:20.79 1.00)
  (:warm 3:31.40 .658)
  (:work 1:53.85  400) ;406
  (:rest 2:09.90  400) ;411
  (:work 1:48.65  400) ;410
  (:rest 2:06.93  400) ;405
  (:work 1:46.40  400) ;402
  (:rest 2:05.47  400) ;411
  (:work 1:44.18  400) ;404
  (:cool 5:24.30 1.00)
  (:cool 4:01.23 .727))

(defworkout "2010-12-05" "10.05"
  (:work 50:00 8.46))

(defworkout "2010-12-06" "11.29"
  (:warm 5:53.92 1.00)
  (:work 5:36.73 1.00)
  (:work 5:27.53 1.00)
  (:work 5:28.91 1.00)
  (:work 5:34.52 1.00)
  (:work 5:23.80 1.00)
  (:work 5:26.09 1.00)
  (:work 5:11.61 1.00)
  (:work 3:20.43 .600))

(defworkout "2010-12-08" "10.19"
  (:warm 5:50.66 1.00)
  (:warm 5:31.73 1.00)
  (:warm 5:18.61 1.00)
  (:work 5:06.18 1.00)
  (:work 5:16.72 1.00)
  (:work 5:11.01 1.00)
  (:work 4:56.54 1.00)
  (:work 4:58.89 1.00)
  (:work 4:50.99 1.00)
  (:cool 3:55.86 .720))

(defworkout "2010-12-10" "10.26"
  (:warm 5:50.26 1.00)
  (:warm 5:31.44 1.00)
  (:warm 5:14.50 1.00)
  (:warm 2:29.47 .453)
  (:work 1:19.42  300) ;293
  (:rest 1:37.18  300) ;301
  (:work 1:14.75  300) ;305
  (:rest 1:39.57  300) ;309
  (:work 1:13.37  300) ;304
  (:rest 1:39.83  300) ;310
  (:work 1:12.20  300) ;304
  (:rest 1:39.45  300) ;307
  (:work 1:13.53  300) ;305
  (:rest 1:39.40  300) ;304
  (:work 1:14.28  300) ;305
  (:rest 1:38.07  300) ;307
  (:work 1:09.42  300) ;316
  ;pause 2:01.57
  (:cool 5:27.86 1.00)
  (:cool 3:00.57 .532))

(defworkout "2010-12-12" "11.53"
  (:warm 5:53.66 1.00)
  (:work 5:39.09 1.00)
  (:work 5:27.49 1.00)
  (:work 5:28.92 1.00)
  (:work 5:22.33 1.00)
  (:work 5:23.67 1.00)
  (:work 5:22.69 1.00)
  (:work 5:15.08 1.00)
  (:work 5:18.03 1.00))

(defworkout "2011-01-09" "11.33"
  (:warm 6:22.45 1.00)
  (:work 5:54.72 1.00)
  (:work 5:49.85 1.00))

(defworkout "2011-01-11" "16.32"
  (:warm 6:01.26 1.00)
  (:work 5:30.52 1.00)
  (:work 5:26.39 1.00)
  (:work 5:29.72 1.00)
  (:work 5:30.10 1.00)
  (:work 5:22.44 1.00))

(defworkout "2011-01-14" "16.15"
  (:warm 6:09.54 1.00)
  (:work 5:42.84 1.00)
  (:work 5:30.20 1.00)
  (:work 5:25.49 1.00)
  (:work 5:14.99 1.00)
  (:work 5:07.83 1.00)
  (:cool 1:06.55 .176))

(defworkout "2011-01-16" "16.15"
  (:warm 6:01.35 1.00)
  (:work 5:36.58 1.00)
  (:work 5:30.36 1.00)
  (:work 5:27.58 1.00)
  (:work 5:16.07 1.00)
  (:work 5:20.33 1.00)
  (:cool 1:12.73 .202))

(defworkout "2011-01-18" "16.21"
  (:warm 6:00    1.00)
  (:work 5:30.57 1.00)
  (:work 5:25.01 1.00)
  (:work 5:14.21 1.00)
  (:work 5:26.31 1.00)
  (:work 4:43.59 1.00)
  (:cool 1:12.94 .201))

(defworkout "2011-01-20" "11.14"
  (:warm 5:55.10 1.00)
  (:work 5:38.35 1.00)
  (:work 5:29.27 1.00)
  (:work 5:20.23 1.00)
  (:work 5:29.51 1.00)
  (:work 5:22.37 1.00)
  (:work 5:21.26 1.00))

(defworkout "2011-01-22" "9.27"
  (:warm 6:10.17 1.00)
  (:work 5:46.89 1.00)
  (:work 5:37.56 1.00)
  (:work 5:35.70 1.00)
  (:work 5:30.93 1.00)
  (:work 5:32.76 1.00)
  (:work 5:28.64 1.00)
  (:work 4:54.46 1.00)
  (:work 5:23.94 1.00))

(defworkout "2011-01-24" "10.53"
  (:warm 5:55.81 1.00)
  (:work 5:33.50 1.00)
  (:work 5:29.60 1.00)
  (:work 5:30.35 1.00)
  (:work 5:24.82 1.00)
  (:work 5:30.55 1.00)
  (:work 5:33.16 1.00))

(defworkout "2011-01-26" "15.48"
  (:warm 6:04.47 1.00 :hr (142 :max 153))
  (:warm 5:36.15 1.00 :hr (154 :max 159))
  (:warm 5:31.76 1.00 :hr (158 :max 160))
  (:work 5:08.38 1.00 :hr (166 :max 170))
  (:work 5:09.37 1.00 :hr (170 :max 176))
  (:work 5:11.58 1.00 :hr (171 :max 174))
  (:work 4:54.23 1.00 :hr (175 :max 177))
  (:cool 5:38.78 .984 :hr (165 :max 175 :end 159)))

(defworkout "2011-01-28" "9.39"
  (:warm 5:37.29  .95)
  (:work 5:34.26 1.00)
  (:work 5:20.06 1.00)
  (:work 5:34.85 1.00)
  (:work 5:19.52 1.00)
  (:work 5:26.99 1.00)
  (:work 5:14.50 1.00))

(defworkout "2011-01-30" "9.43"
  (:warm 6:02.04 1.00)
  (:work 5:40.27 1.00)
  (:work 5:32.32 1.00)
  (:work 5:27.51 1.00)
  (:work 5:18.08 1.00)
  (:work 5:36.64 1.00)
  (:work 5:21.17 1.00)
  (:work 5:34.71 1.00)
  (:work 5:08.54 1.00))

(defworkout "2011-02-01" "10.44"
  (:warm 5:57.25 1.00)
  (:work 5:35.28 1.00)
  (:work 5:14.37 1.00)
  (:work 5:30.80 1.00)
  (:work 5:10.42 1.00)
  (:work 5:37.52 1.00))

(defworkout "2011-02-03" "16.00"
  (:warm 6:05.50 1.00)
  (:warm 5:40.59 1.00)
  (:warm 5:29.04 1.00)
  (:warm 5:28.59 1.00)
  (:work 5:13.41 1.00)
  (:work 5:09.21 1.00)
  (:work 1:21.65 .260) ;5:14/km
  (:rest 5:05.88    0)
  (:work 4:57.29 1.00)
  (:work 4:47.20 1.00)
  (:work 3:30.68 .749))

(defworkout "2011-02-05" "9.46"
  (:warm 6:12.70 1.00)
  (:work 5:36.87 1.00)
  (:work 5:57.27 1.00)
  (:work 5:32.40 1.00)
  (:work 5:25.54 1.00)
  (:work 5:17.76 1.00)
  (:work 5:21.27 1.00)
  (:work 5:09.67 1.00)
  (:work 5:07.20 1.00)
  (:work 5:13.39 1.00))

(defworkout "2011-02-07" "14.57"
  (:warm 5:57.51 1.00)
  (:work 5:36.32 1.00)
  (:work 5:24.42 1.00)
  (:work 5:19.89 1.00)
  (:work 5:27.07 1.00)
  (:work 5:21.23 1.00)
  (:work 5:08.56 1.00))

(defworkout "2011-02-09" "10.07"
  (:warm 6:09.06 1.00)
  (:warm 5:40.47 1.00)
  (:warm 5:23.00 1.00)
  (:work 4:53.44 1.00)
  (:rest 0:58.67    0) ;115m
  (:work 4:52.38 1.00)
  (:rest 0:59.62    0) ;118m
  (:work 4:47.19 1.00)
  (:rest 1:00.44    0) ;115m
  (:work 4:42.32 1.00)
  (:cool 5:54.21 1.00))

(defworkout "2011-02-11" "15.25"
  (:warm 5:57.74 1.00)
  (:warm 5:29.67 1.00)
  (:warm 5:24.33 1.00)
  (:warm 5:41.83 1.00)
  (:warm 5:43.03 1.00)
  (:warm 2:53.37 .504)
  (:rest 0:56.30    0)
  (:work 0:49.57  210) ;220.4
  (:rest 2:49.90    0)
  (:work 0:45.20  205) ;210.8
  (:rest 3:32       0)
  (:work 0:46.52  205) ;211.5
  (:rest 2:43.28    0)
  (:work 0:40.95  200) ;206.6
  (:rest 1:47.77    0)
  (:cool 4:30.09 .801))

(defworkout "2011-02-14" "10.34"
  (:warm 6:00    1.00) ;5:35.52
  (:work 5:31.25 1.00)
  (:work 5:27.07 1.00)
  (:work 5:20.23 1.00)
  (:work 5:02.68 1.00)
  (:work 5:30.82 1.00)
  (:cool 0:55.23 .16))

(defworkout "2011-02-18" "16.46"
  (:warm 5:47.09 1.00)
  (:work 5:28.32 1.00)
  (:work 5:19.67 1.00)
  (:work 5:21.43 1.00)
  (:work 5:06.40 1.00)
  (:work 5:29.51 1.00))

(defworkout "2011-02-16" "10.12"
  (:warm 5:53.57 1.00)
  (:warm 5:31.55 1.00)
  (:warm 5:27.58 1.00)
  (:work 5:13.54 1.00)
  (:work 5:01.60 1.00)
  (:work 5:14.20 1.00)
  (:work 4:45.93 1.00)
  (:work 4:59.16 1.00)
  (:cool 5:23.73 1.00))

(defworkout "2011-02-21" "15.48"
  (:warm 5:58.21 1.00)
  (:warm 5:35.49 1.00)
  (:warm 5:26.11 1.00)
  (:warm 5:22.19 1.00)
  (:warm 5:26.66 1.00)
  (:warm 1:47.66 .349)
  (:work 0:50.57  200) ;197.3
  (:rest 1:02.05    0)
  (:work 0:44.80  200) ;210.7
  (:rest 1:02.88    0)
  (:work 0:44.52  200) ;203.6
  (:rest 1:01.30    0)
  (:work 0:40.78  200) ;211.1
  (:rest 1:02.90    0)
  (:work 0:42.97  200) ;194.9
  (:rest 4:58.63    0)
  (:cool 4:00.05 .697))

(defworkout "2011-02-23" "10.19"
  (:warm 5:50.15 1.00)
  (:warm 5:29.94 1.00)
  (:warm 5:23.15 1.00)
  (:work 5:03.15 1.00)
  (:work 4:59.32 1.00)
  (:work 4:57.74 1.00)
  (:work 4:47.30 1.00)
  (:cool 5:19.61 1.00))

(defworkout "2011-02-25" "10.41"
  (:warm 5:57.95 1.00)
  (:work 5:24.37 1.00)
  (:work 5:23.04 1.00)
  (:work 5:17.74 1.00)
  (:work 5:12.32 1.00)
  (:work 5:05.76 1.00)
  (:work 5:30    1.00))

(defworkout "2011-02-27" "9.08"
  (:warm 6:02.99 1.00)
  (:work 5:40.74 1.00)
  (:work 5:32.43 1.00)
  (:work 5:31.86 1.00)
  (:work 5:23.15 1.00)
  (:work 5:28.64 1.00)
  (:work 5:32.02 1.00)
  (:work 5:32.71 1.00)
  (:work 5:33.49 1.00)
  (:work 5:18.00 1.00))

(defworkout "2011-03-02" "10.08"
  (:warm 6:03.48 1.00)
  (:warm 5:38.39 1.00)
  (:warm 5:27.06 1.00)
  (:work 5:13.20 1.00)
  (:work 5:01.17 1.00)
  (:work 5:04.15 1.00)
  (:work 4:56.73 1.00)
  (:work 4:50.74 1.00)
  (:work 4:38.24 1.00)
  (:cool 1:40.51 .303))

(defworkout "2011-03-04" "15.47"
  (:warm 5:45.97 1.00)
  (:warm 5:28.37 1.00)
  (:warm 5:24.01 1.00)
  (:warm 5:17.77 1.00)
  (:warm 5:19.26 1.00)
  (:warm 1:37.51 .309)
  ;pause 2:22.05
  (:work 0:45.03  200) ;194.7
  (:rest 1:55.95    0)
  (:work 0:41.67  200) ;214.7
  (:rest 1:56.03    0)
  (:work 0:38.67  200));204.7

(defworkout "2011-03-06" "10.36"
  (:warm 5:57.63 1.00)
  (:work 5:39.52 1.00)
  (:work 5:39.16 1.00)
  (:work 5:28.08 1.00)
  (:work 5:34.64 1.00)
  (:work 5:32.40 1.00)
  (:work 5:36.87 1.00)
  (:work 5:40.29 1.00)
  (:work 5:36.54 1.00)
  (:work 5:34.57 1.00)
  (:work 5:31.32 1.00))

(defworkout "2011-03-07" "15.58"
  (:warm 5:41.18 1.00)
  (:work 5:29.91 1.00)
  (:work 5:16.85 1.00)
  (:work 5:15.10 1.00)
  (:work 5:18.61 1.00)
  (:work 5:21.26 1.00))

(defworkout "2011-03-09" "9.17"
  (:warm 5:59.08 1.00)
  (:warm 5:41.94 1.00)
  (:warm 5:20.29 1.00)
  ;pause 0:20.18
  (:work 5:10.38 1.00)
  (:rest 0:58.65    0)
  (:work 4:45.57 1.00)
  (:rest 1:00.00    0)
  (:work 5:01.63 1.00)
  (:rest 1:00.95    0)
  (:work 4:33.88 1.00)
  ;pause 7:18.12
  (:work 4:04.74 1.00))

(defworkout "2011-03-11" "14.58"
  (:warm 5:49.67 1.00)
  (:warm 5:34.09 1.00)
  (:warm 5:26.46 1.00)
  (:warm 5:22.78 1.00)
  (:warm 4:12.56 .776)
  ;pause 6:40.77
  (:warm 4:17.70 .772)
  ;pause 2:00.83
  (:work 0:44.15  200) ;214.3
  (:rest 1:57.40    0)
  (:work 0:39.07  200) ;197.6
  (:rest 1:57.36    0)
  (:work 0:38.84  200) ;215.2
  (:rest 1:59.03    0)
  (:work 0:37.25  200) ;193.9
  ;pause 1:57.90
  (:cool 2:46.87 .505))

(defworkout "2011-03-13" "9.28"
  (:warm 6:09.57 1.00)
  (:work 5:40.05 1.00)
  (:work 5:43.77 1.00)
  (:work 5:31.02 1.00)
  (:work 5:32.46 1.00)
  (:work 5:22.42 1.00)
  (:work 5:36.10 1.00)
  (:work 5:40.39 1.00)
  (:work 5:37.35 1.00)
  (:work 5:36.47 1.00)
  (:work 5:37.10 1.00))

(defworkout "2011-03-16" "9.56"
  (:warm 5:38.37  .95)
  (:warm 5:40.09 1.00)
  (:warm 5:30.32 1.00)
  (:work 5:15.78 1.00)
  (:work 5:08.53 1.00)
  (:work 5:04.70 1.00)
  (:work 4:53.52 1.00)
  (:work 4:48.17 1.00)
  (:work 4:45.42 1.00))

(defworkout "2011-03-18" "16.34"
  (:warm 5:48.94 1.00)
  (:warm 5:30.46 1.00)
  (:warm 5:26.74 1.00)
  (:warm 5:18.04 1.00)
  (:warm 5:15.87 1.00)
  (:warm 2:47.27 .507)
  ;pause 0:42.10
  (:work 0:49.57  200) ;205.7
  (:rest 0:58.08    0)
  (:work 0:47.82  200) ;205.0
  (:rest 1:00.90    0)
  (:work 0:45.90  200) ;210.7
  (:rest 0:59.03    0)
  (:work 0:44.52  200) ;201.1
  (:rest 0:59.28    0)
  (:work 0:43.02  200) ;207.5
  (:rest 0:58.75    0)
  (:work 0:41.35  200) ;205.2
  (:rest 0:59.55    0)
  (:work 0:41.70  200) ;205.7
  (:rest 1:00.95    0)
  (:work 0:38.23  200) ;199.8
  ;pause 1:05.00
  (:cool 2:48.75 .502))

(defworkout "2011-03-20" "9.20"
  (:warm 6:02.33 1.00)
  (:work 5:41.68 1.00)
  (:work 5:37.73 1.00)
  (:work 5:36.22 1.00)
  (:work 5:34.03 1.00)
  (:work 5:28.70 1.00)
  (:work 5:32.72 1.00)
  (:work 5:35.14 1.00)
  (:work 5:31.99 1.00)
  (:work 5:33.96 1.00)
  (:work 5:37.24 1.00)
  (:work 1:07.65 .202))

(defworkout "2011-03-21" "10.12"
  (:warm 5:35.16  .95)
  (:work 5:37.65 1.00)
  (:work 5:18.94 1.00)
  (:work 4:50.54 .902)
  ;pause 2:27.53
  (:hill 0:16.57   55) ;58
  (:rest 1:12.98    0)
  (:hill 0:15.10   55) ;54
  (:rest 1:17.52    0)
  (:hill 0:12.98   55) ;59
  (:rest 1:28.00    0)
  (:hill 0:12.09   55) ;56
  (:rest 1:31.18    0)
  (:hill 0:12.90   55) ;59
  (:rest 1:30.95    0)
  (:hill 0:12.10   55) ;57
  ;pause 8:36.50
  (:work 5:37.71 1.00))

(defworkout "2011-03-23" "10.30"
  (:warm 5:38.38  .95)
  (:warm 5:31.45 1.00)
  (:warm 5:15.01 1.00)
  (:warm 0:49.70 .150)
  (:work 4:51.55 1.00)
  (:rest 1:01.80    0)
  (:work 4:51.46 1.00)
  (:rest 0:59.47    0)
  (:work 4:51.79 1.00)
  (:rest 1:00.93    0)
  (:work 4:48.75 1.00)
  (:rest 1:00.18    0)
  (:work 4:45.83 1.00)
  (:rest 1:00.44    0)
  (:work 3:44.50 .798)
  (:work 0:48.48 .211)
  ;pause 3:13.90
  (:cool 2:52.20 .504))

(defworkout "2011-03-25" "16.11"
  (:warm 5:49.25 1.00)
  (:warm 5:37.75 1.00)
  (:warm 5:24.57 1.00)
  (:warm 2:37.52 .481)
  (:rest 1:28.95    0)
  (:work 0:10.40   50) ;55.4
  (:rest 1:19.40    0)
  (:work 0:09.25   50) ;45.7
  (:rest 1:28.45    0)
  (:work 0:08.63   50) ;46.1
  (:rest 1:08.97    0)
  (:work 0:08.75   50) ;50.6
  (:cool 3:16.25 .581))

(defworkout "2011-03-27" "10.46"
  (:warm 6:03.42 1.00)
  (:work 5:46.56 1.00)
  (:work 5:37.47 1.00)
  (:work 5:33.89 1.00)
  (:work 5:32.00 1.00)
  (:work 5:25.63 1.00)
  (:work 5:31.15 1.00)
  (:work 5:35.08 1.00)
  (:work 3:10.19 .556)
  ;pause 4:16.83
  (:cool 5:51.90 1.00))

(defworkout "2011-03-28" "9.58"
  (:warm 5:58.15 1.00)
  (:work 5:34.82 1.00)
  (:work 5:18.33 1.00)
  (:work 4:23.77 1.00)
  (:work 2:06.25 .373)
  (:hill 0:16.50   55) ;54
  (:rest 1:55.32    0)
  (:hill 0:13.62   55) ;56
  (:rest 1:56.73    0)
  (:hill 0:13.20   55) ;54
  (:rest 1:58.56    0)
  (:hill 0:12.32   55) ;57
  (:rest 1:39.90    0)
  (:work 5:35.02 1.00)
  (:work 1:30.08 .262))

(defworkout "2011-03-30" "10.30"
  (:warm 5:49.97 1.00)
  (:warm 5:37.49 1.00)
  (:warm 5:24.38 1.00)
  (:warm 5:17.19 1.00)
  (:work 4:50.72 1.00)
  (:rest 0:57.44    0)
  (:work 4:46.23 1.00)
  (:rest 1:00.00    0)
  (:work 4:41.80 1.00)
  (:rest 1:00.87    0)
  (:rest 1:00.44    0)
  (:work 3:45     .80)
  (:work 0:46.26  .20)
  ;pause 2:01.59
  (:cool 5:30.10 1.00)
  (:cool 4:27.88 .801))

(defworkout "2011-04-01" "15.27"
  (:warm 5:47.82 1.00)
  (:warm 5:32.71 1.00)
  (:warm 5:25.53 1.00)
  (:warm 5:26.00 1.00)
  (:warm 5:18.90 1.00)
  (:warm 1:30.62 .304)
  ;pause 6:22.25
  (:work 0:43.98  200) ;209.5
  (:rest 1:01.12    0)
  (:work 0:42.68  200) ;203.1
  (:rest 0:59.67    0)
  (:work 0:41.83  200) ;213.3
  (:rest 0:58.52    0)
  (:work 0:41.75  200) ;200.6
  (:rest 0:57.88    0)
  (:work 0:42.20  200) ;202.7
  (:rest 0:58.85    0)
  (:work 0:42.20  200) ;200.6
  (:rest 0:58.62    0)
  (:work 0:42.05  200) ;210.0
  (:rest 1:00.13    0)
  (:work 0:36.75  200));185.6

(defworkout "2011-04-03" "9.53"
  (:warm 6:02.18 1.00)
  (:work 5:41.10 1.00)
  (:work 5:38.54 1.00)
  (:work 5:37.75 1.00)
  (:work 5:31.73 1.00)
  (:work 5:37.50 1.00)
  (:work 5:42.48 1.00)
  (:work 5:32.62 1.00)
  (:work 5:34.40 1.00)
  (:work 5:33.35 1.00))

(defworkout "2011-04-06" "10.43"
  (:warm 6:03.36 1.00)
  (:warm 5:37.01 1.00)
  (:warm 5:30.95 1.00)
  (:warm 5:05.95 1.00)
  ;pause 2:02.47
  (:work 4:31.51 1.00)
  (:rest 2:00.29    0)
  (:work 4:30.17 1.00)
  (:rest 1:59.88    0)
  (:work 4:28.42 1.00))

(defworkout "2011-04-08" "16.17"
  (:warm 5:49.8 1000)
  (:warm 5:37.4 1000)
  (:warm 5:35.1 1000)
  (:warm 5:16.9 1000)
  (:warm 5:27.9 1000)
  ;pause 15:18.6
  (:work 0:35.3  200)
  (:rest 5:28.7    0)
  (:work 0:35.7  200)
  (:rest 7:57.4    0)
  (:work 0:35.1  200)
  (:rest 5:45.6    0)
  (:work 0:31.9  200))

(defworkout "2011-04-10" "10.45"
  (:warm 6:01.22 1.00)
  (:warm 5:40.08 1.00)
  (:warm 5:32.98 1.00)
  (:work 5:04.22 1.00)
  (:work 5:05.24 1.00)
  (:work 5:01.76 1.00)
  (:work 2:38.54 .535))

(defworkout "2011-05-03" "17.00"
  (:work 0:13.1 100))

(defworkout "2011-05-10" "19.00"
  (:work 0:27.5 200))

(defworkout "2011-05-16" "19.00"
  (:work 0:62.1 400))

(defworkout "2011-05-24" "19.00"
  (:work 0:43.42 300))

(defworkout "2011-05-28" "11.45"
  (:warm 3:12.7 0.50)
  (:warm 3:03.4 0.50))

(defworkout "2011-06-01" "12.30"
  (:warm 3:08.7 0.50)
  (:warm 3:04.6 0.50)
  (:work 2:53.6 0.50))

(defworkout "2011-06-05" "10.00"
  (:warm 3:14.9 0.50)
  (:warm 3:00.6 0.50))

(defworkout "2011-06-29" "17.00"
  (:work 0:66 0.40)
  (:rest 35)
  (:work 0:59 0.40))

(defworkout "2011-07-04" "15.00"
  (:warm 0:10.6  60)
  (:warm 0:12.7  80)
  (:work 0:45.2 300)
  ;pause 22:10
  (:work 1:31.7 500))
  ;:cool ? 0.30 barefeet

(defworkout "2011-07-09" "18.45"
  (:warm 6:23.6 1000)
  ;pause
  (:warm 0:15.8  100)
  ;pause
  (:work 0:61.72 400))

(defworkout "2011-07-13" "15.08"
  (:warm 5:59.3 1000)
  (:rest 0:59.9    0)
  (:work 5:40.2 1000)
  ;pause 4:47.8
  (:work 0:19.6  100))

(defworkout "2011-07-15" "14.42"
  (:warm 6:05.7 1.00)
  (:rest 0:57.7    0)
  (:work 5:23.2 1.00))

(defworkout "2011-07-18" "15.14"
  (:warm 6:03.46 1.00)
  (:work 5:45.26 1.00)
  (:work 5:42.77 1.00))

(defworkout "2011-07-20" "14.45"
  (:warm 3:00 500)) ;Barefeet

(defworkout "2011-07-22" "15.58"
  (:warm 6:16.78 1.00)
  (:work 5:43.21 1.00)
  (:work 5:30.43 1.00))

(defworkout "2011-07-27" "14.19"
  (:warm 2:31.6 400)
  (:rest 0:29.6   0)
  (:work 2:21.1 400)
  (:rest 0:28.4   0)
  (:work 2:14.5 400)
  (:rest 0:29.6   0)
  (:work 2:09.7 400)
  (:rest 0:31.4   0)
  (:work 2:03.2 400)
  (:rest 0:29.5   0)
  (:work 2:01.6 400)
  (:rest 0:29.2   0)
  (:work 1:54.8 400)
  (:rest 0:30.0   0)
  (:work 1:43.2 400)
  (:rest 1:00.2   0)
  (:cool 2:27.5 400))

(defworkout "2011-07-29" "10.15"
  (:warm 6:10.25 1.00)
  (:work 5:45.98 1.00)
  (:work 5:25.98 1.00)
  (:work 2:02.98 .404))

(defworkout "2011-08-01" "9.18"
  (:warm 6:08.89 1.00)
  (:work 5:46.64 1.00)
  (:work 5:11.88 1.00)
  (:work 3:51.76 .700))

(defworkout "2011-08-03" "9.33"
  (:warm 5:59.44 1.00)
  (:work 5:42.31 1.00)
  (:work 5:39.65 1.00)
  (:work 5:10.59 1.00)
  (:cool 1:13.71 .202))

(defworkout "2011-08-05" "9.14"
  (:warm 6:01.12 1.00)
  (:work 5:43.33 1.00)
  (:work 5:23.99 1.00)
  (:work 4:53.58 1.00)
  (:cool 1:20.78 .207))

(defworkout "2011-08-08" "9.26"
  (:warm 6:06.20 1.00)
  (:work 5:53.61 1.00)
  (:work 5:25.07 1.00)
  (:work 4:52.98 1.00)
  (:work 5:28.08 1.00))

(defworkout "2011-08-10" "9.43"
  (:warm 5:56.84 1.00)
  (:work 0:55.63 .159)
  ;pause 3:27.07
  (:work 5:29.79 1.00)
  (:work 2:26.51 .504)
  ;pause 2:26.65
  (:hill 0:13.68 .060) ;59m
  (:rest 1:18.12    0)
  (:hill 0:12.62 .060) ;63
  (:rest 1:18.26    0)
  (:hill 0:11.20 .060) ;64
  (:rest 1:19.55    0)
  (:hill 0:11.05 .060) ;58
  (:rest 1:18.92    0)
  (:hill 0:11.60 .060) ;58
  ;pause 1:12.85
  (:work 5:16.28 1.00)
  (:work 1:13.95 .255))

(defworkout "2011-08-12" "9.37"
  (:warm 5:58.26 1.00)
  (:work 5:21.43 1.00)
  (:work 1:07.95 .200)
  ;pause 3:32.55
  (:work 2:33.45  500)
  ;pause 2:36.03
  (:work 5:12.79 1.00))

(defworkout "2011-08-15" "16.59"
  (:warm 6:18.2 1000)
  (:work 5:19.2 1000)
  ;pause 4:24.4
  (:work 0:08.8   40)
  (:rest 2:24.9    0)
  (:work 0:07.8   40)
  (:rest 3:15.7    0)
  (:work 0:06.5   40)
  (:rest 4:41.4    0)
  (:work 0:11.3   40)
  (:rest 2:15.3    0)
  (:work 0:07.9   40))

(defworkout "2011-08-16" "16.42"
  (:warm  6:48.7 1200)
  ;pause 20:40.2
  (:warm  0:15.1  100)
  ;pause
  (:work  0:13.0  100)) ;Officiellt 12,8m

(defworkout "2011-08-18" "9.43"
  (:warm 5:56.37 1.00)
  (:work 5:35.64 1.00)
  (:work 5:26.88 1.00)
  (:work 5:02.39 .924) ;1010m
  (:work 0:25.03 .076) ;  79m
  (:work 5:23.28 1.00)
  (:cool 1:18.23 .216))

(defworkout "2011-08-21" "12.21"
  (:warm 6:05.43 1.00)
  (:work 5:28.46 1.00)
  (:work 5:21.26 1.00)
  (:work 5:02.35 1.00)
  (:work 4:54.28 1.00)
  (:work 5:36.43 1.00))

(defworkout "2011-09-07" "17.49"
  (:warm 6:03.43 1.00)
  (:work 5:41.36 1.00)
  (:work 5:09.25 1.00)
  (:work 4:31.30 1.00))

(defworkout "2011-09-09" "10.34"
  (:warm 5:52.77 1.00)
  (:work 5:45.01 1.00)
  (:work 5:33.95 1.00)
  (:work 5:24.34 1.00)
  (:work 5:01.96 1.00)
  (:work 5:29.55 1.00))

(defworkout "2011-09-12" "16.09"
  (:warm 2:22.0 400)
  (:rest 1:13.1 200)
  (:warm 2:15.5 400)
  (:rest 1:10.9 200)
  (:warm 2:07.7 400)
  (:rest 1:10.6 200)
  (:warm 2:07.6 400)
  (:rest 1:09.9 200)
  (:work 1:58.8 400)
  (:rest 1:09.4 200)
  (:work 1:54.1 400)
  (:rest 1:10.8 200)
  (:work 1:53.8 400)
  (:rest 1:09.1 200)
  (:work 1:45.0 400))

(defworkout "2011-09-14" "10.02"
  (:warm 6:13.52 1.00)
  (:work 5:50.06 1.00)
  (:work 5:28.59 1.00)
  (:work 5:35.55 1.00)
  (:work 5:23.45 1.00)
  (:work 5:01.98 1.00)
  (:work 5:32.97 1.00))

(defworkout "2011-09-16" "9.43"
  (:warm 5:46.42 1.00)
  (:work 5:30.30 1.00)
  (:work 0:43.35 .138)
  ;pause 0:58.47
  (:hill 0:15.13 .060) ;55m
  (:rest 1:26.17    0)
  (:hill 0:13.75 .060) ;61
  (:rest 1:26.40    0)
  (:hill 0:13.25 .060) ;61
  (:rest 1:29.45    0)
  (:hill 0:12.00 .060) ;59
  (:rest 1:26.80    0)
  (:hill 0:10.35 .060) ;49
  ;pause 1:22.03
  (:work 5:27.49 1.00)
  (:work 1:25.31 .25))

(defworkout "2011-09-19" "9.40"
  (:warm 6:00.96 1.00)
  (:work 5:39.53 1.00)
  (:work 5:22.76 1.00)
  (:work 5:08.19 1.00)
  (:work 5:15.47 1.00)
  (:work 4:48.94 1.00)
  (:work 5:30    1.00))

(defworkout "2011-09-21" "10.27"
  (:warm 5:43.57 1.00)
  (:work 5:29.71 1.00)
  (:work 5:01.81 1.00)
  ;pause 1:27.00
  (:hill 0:12.70 .060) ;57m
  (:rest 1:36.38    0)
  (:hill 0:11.92 .060) ;57
  (:rest 1:37.18    0)
  (:hill 0:11.30 .060) ;66
  (:rest 1:39.55    0)
  (:hill 0:10.85 .060) ;57
  (:rest 1:40.92    0)
  (:hill 0:09.98 .060) ;52
  ;pause 1:42.22
  (:work 5:29.45 1.00)
  (:work 1:48.60 .32))

(defworkout "2011-09-23" "9.26"
  (:warm 5:56.16 1.00)
  (:warm 1:12.81 .200)
  (:warm 5:05.49  964)
  (:warm 0:11.46   36)
  ;pause 1:35.82
  (:work 3:54.40  800)
  (:rest 0:45.50    0)
  (:work 3:49.45  800)
  (:rest 0:44.68    0)
  (:work 3:48.62  800)
  (:rest 0:46.24    0)
  (:work 3:37.21  800)
  ;pause 1:24.60
  (:cool 5:21.28 1.00))

(defworkout "2011-09-28" "15.23"
  (:warm 5:59.21 1.00)
  (:work 5:33.96 1.00)
  (:work 5:21.87 1.00)
  (:work 4:58.71 1.00)
  (:work 4:51.85 1.00))

(defworkout "2011-09-30" "17.30"
  (:warm 5:42.2 1000)
  (:warm 2:39.7  500)
  ;pause 2:40
  (:work 1:52.3  400)
  (:rest 1:12.6  200)
  (:work 1:53.8  400)
  (:rest 1:16.7  200)
  (:work 1:59.1  400)
  (:rest 1:13.1  200)
  (:work 1:54.2  400)
  (:rest 1:13.3  200)
  (:work 1:50.7  400)
  (:rest 1:14.6  200)
  (:work 1:48.6  400)
  (:rest 1:12.8  200)
  ;pause
  (:cool 5:50.4 1000))

(defworkout "2011-10-03" "15.53"
  (:warm 5:46.72 1.00)
  (:work 5:22.25 1.00)
  (:work 0:39.20 .106)
  ;pause 3:29.50
  (:hill 0:09.14   40) ;45.1
  (:rest 1:45.91    0)
  (:hill 0:08.74   40) ;44.7
  (:rest 1:45.23    0)
  (:hill 0:08.62   40) ;41.9
  (:rest 1:44.97    0)
  (:hill 0:08.79   40) ;43.5
  (:rest 1:43.12    0)
  (:hill 0:08.45   40) ;47.9
  (:rest 1:46.43    0)
  (:hill 0:08.42   40) ;50.6
  ;pause 1:53.20
  (:work 5:21.61 1.00)
  (:work 3:14.59 .58))

(defworkout "2011-10-05" "15.13"
  (:warm 3:00.52 .500)
  (:warm 3:00.12 .500)
  (:work 5:36.14 1.00)
  (:work 5:31.03 1.00)
  (:work 5:25.29 1.00)
  (:work 5:34.35 1.00)
  (:work 5:23.69 1.00))

(defworkout "2011-10-07" "9.48"
  (:warm 6:11.82 1.00)
  (:warm 5:49.27 1.00)
  (:warm 5:46.30 1.00)
  (:warm 5:27.54 1.00)
  (:work 5:14.91 1.00)
  (:work 5:06.12 1.00)
  (:work 4:53.41 1.00)
  (:work 2:19.97 .405))

(defworkout "2011-10-10" "8.55"
  (:warm 6:01.02 1.00)
  (:work 5:39.43 1.00)
  (:work 2:55.24 .549)
  ;pause 2:40.92
  (:hill 0:08.88   40) ;51.1
  (:rest 1:55.47    0)
  (:hill 0:09.09   40) ;49.0
  (:rest 1:57.76    0)
  (:hill 0:08.43   40) ;44.4
  (:rest 2:02.80    0)
  (:hill 0:08.65   40) ;54.5
  (:rest 1:58.77    0)
  (:hill 0:08.68   40) ;39.6
  (:rest 2:06.60    0)
  (:hill 0:08.72   40) ;47.6
  ;pause 2:24.30
  (:work 5:39.62 1.00)
  (:work 2:58.03 .54))

(defworkout "2011-10-12" "12.13"
  (:warm 3:00.54 .50)
  (:work 5:40.75 1.00)
  (:work 5:33.58 1.00)
  (:work 5:31.66 1.00)
  (:work 5:27.97 1.00)
  (:work 5:36.70 1.00)
  (:work 5:31.49 1.00)
  (:work 5:31.63 1.00)
  (:cool 3:24.47 .578))

(defworkout "2011-10-14" "16.01"
  (:warm 5:59.4 1000)
  (:warm 5:22.4 1000)
  ;pause 4:10
  (:work 3:50.8  800)
  (:rest 1:00.3    0)
  (:work 3:49.5  800)
  (:rest 1:00.6    0)
  (:work 3:45.3  800)
  (:rest 0:59.7    0)
  (:work 3:45.5  800)
  (:rest 1:00.0    0)
  (:work 3:28.3  800)
  ;pause 6:11.6
  (:work 0:18.5  100)
  (:rest 3:35.3    0)
  (:work 0:14.9  100))

(defworkout "2011-10-18" "10.16"
  (:warm 5:55.70 1.00)
  (:work 5:45.39 1.00)
  (:work 5:42.65 1.00)
  (:work 5:43.73 1.00)
  (:work 5:38.26 1.00)
  (:work 5:32.15 1.00)
  (:work 5:18.31 1.00)
  (:work 5:16.28 1.00)
  (:work 5:19.86 1.00))

(defworkout "2011-10-20" "9.54"
  (:warm 6:04.03 1.00)
  (:work 5:41.22 1.00)
  (:work 5:32.67 1.00)
  (:work 5:28.72 1.00)
  (:work 5:21.96 1.00)
  (:work 5:28.83 1.00)
  ;pause 8:29.24
  (:work 5:17.93 1.00)
  (:work 4:58.37 1.00)
  (:work 5:26.59 1.00))

(defworkout "2011-10-22" "10.01"
  (:warm 5:50.0 1000)
  (:warm 5:33.0 1000)
  ;pause 5:30
  (:work 4:53.2 1000)
  (:rest 0:59.7    0)
  (:work 4:42.2 1000)
  (:rest 1:00.1    0)
  (:work 4:38.6 1000)
  (:rest 1:01.8    0)
  (:work 4:25.6 1000))

(defworkout "2011-10-24" "11.03"
  (:warm 5:55.07 1.00)
  (:work 5:42.40 1.00)
  (:work 5:24.18 1.00)
  (:work 2:53.25 .601)
  ;pause 3:27.13
  (:hill 0:10.85   60) ;54
  (:rest 2:00.00    0)
  (:hill 0:10.27   60) ;55
  (:rest 2:01.93    0)
  (:hill 0:10.32   60) ;58
  (:rest 2:30.40    0)
  (:hill 0:10.45   60) ;53
  (:rest 2:28.53    0)
  (:hill 0:10.07   60) ;62
  ;pause 2:34.85
  (:work 5:33.87 1.00))

(defworkout "2011-10-26" "9.52"
  (:warm 5:48.37 1.00)
  (:work 5:45.08 1.00)
  (:work 5:38.78 1.00)
  (:work 5:34.42 1.00)
  (:work 5:30.37 1.00)
  (:work 5:31.23 1.00)
  (:work 5:20.59 1.00)
  (:work 5:16.83 1.00)
  ;pause 4:05.70
  (:work 5:20.52 1.00)
  (:work 1:33.98 .302))

(defworkout "2011-10-28" "16.27"
  (:warm 5:58.6 1000)
  (:warm 5:31.4 1000)
  ;pause 3:30.3
  (:warm 0:21.3  100)
  (:rest 0:40.1    0)
  (:warm 0:20.1  100)
  (:rest 1:02.1    0)
  (:warm 0:18.0  100)
  ;pause 3:10
  (:work 1:45.7  400)
  (:rest 0:58.7    0)
  (:work 1:43.4  400)
  (:rest 0:58.4    0)
  (:work 1:44.3  400)
  (:rest 0:59.9    0)
  (:work 1:43.8  400)
  (:rest 1:00.0    0)
  (:work 1:44.0  400)
  (:rest 0:59.0    0)
  (:work 1:44.2  400))

(defworkout "2011-10-31" "15.15"
  (:warm 5:50.88 1.00)
  (:work 5:35.89 1.00)
  (:work 5:26.82 1.00)
  (:work 4:53.21 1.00)
  ;pause 4:00.51
  (:hill 0:08.98   40) ;48.0
  (:rest 2:11.16    0)
  (:hill 0:08.77   40) ;40.9
  (:rest 2:23.50    0)
  (:hill 0:08.73   40) ;50.4
  (:rest 2:25.74    0)
  (:hill 0:08.35   40) ;48.1
  (:rest 2:30.65    0)
  (:hill 0:08.4    40) ;?
  (:rest 2:27.99    0)
  (:hill 0:08.33   40) ;41.7
  ;pause 2:54.78
  (:work 5:33.40 1.00)
  (:work 2:55.57  .52));?

(defworkout "2011-11-04" "15.36"
  (:warm 5:52.45 1.00)
  (:work 5:39.23 1.00)
  (:work 5:30.14 1.00)
  (:work 5:31.71 1.00)
  (:work 5:22.35 1.00)
  (:work 5:35.80 1.00)
  (:work 5:35.13 1.00)
  (:work 5:43.89 1.00)
  (:work 5:41.09 1.00))

(defworkout "2011-11-07" "10.11"
  (:warm 6:07.14 1.00)
  (:work 5:51.61 1.00)
  (:work 5:39.19 1.00)
  (:work 2:34.23 .500)
  ;pause 4:39.78
  (:hill 0:10.31   60) ;55
  (:rest 2:25.78    0)
  (:hill 0:10.00   60) ;52
  (:rest 2:26.78    0)
  (:hill 0:10.35   60) ;55
  (:rest 2:26.22    0)
  (:hill 0:10.07   60) ;56
  (:rest 2:28.46    0)
  (:hill 0:09.62   60) ;55
  ;pause 2:32.60
  (:work 5:45.56 1.00)
  (:work 1:07.62 .198))

(defworkout "2011-11-09" "11.04"
  (:warm 6:01.83 1.00)
  (:work 6:00.71 1.00)
  (:work 5:50.02 1.00)
  (:work 5:41.89 1.00)
  (:work 5:39.12 1.00)
  (:work 5:32.01 1.00)
  (:work 5:37.62 1.00)
  (:work 5:20.06 1.00)
  (:work 5:32.20 1.00)
  (:work 5:02.27  .93)) ;?

(defworkout "2011-11-11" "11.04"
  (:warm 6:00.57 1.00)
  (:work 0:51.24 .150)
  (:work 5:26.23 1000)
  ;pause 3:28.75
  (:work 4:49.00 1000)
  (:rest 1:00.05    0)
  (:work 3:14.13  666)
  ;pause 4:28.37
  (:work 5:26.89 1.00))

(defworkout "2011-11-16" "10.38"
  (:warm 6:02.18 1.00)
  (:work 6:05.28 1.00)
  (:work 5:38.72 1.00)
  (:work 5:35.31 1.00)
  (:work 5:26.61 1.00)
  (:work 5:44.36 1.00)
  (:work 5:20.55 1.00)
  (:work 5:15.55 1.00)
  (:work 5:37.84 1.00))

(defworkout "2011-11-18" "10.56"
  (:warm 5:57.04 1.00)
  (:work 5:36.93 1.00)
  (:work 5:45.70 1.00)
  (:work 5:39.10 1.00)
  (:work 5:30.00 1.00)
  (:work 5:35.98 1.00)
  (:work 5:25.69 1.00)
  (:work 5:20.67 1.00)
  (:work 5:37.06 1.00)
  (:work 5:40    1.00))

(defworkout "2011-11-20" "11.52"
  (:warm 6:00.2 1000)
  (:warm 5:31.9 1000)
  ;pause 7:22.4
  (:work 0:41.4  200)
  (:rest 1:58.3    0)
  (:work 0:36.9  200)
  (:rest 1:58.2    0)
  (:work 0:33.6  200))

(defworkout "2011-11-23" "9.58"
  (:warm 6:12.52 1.00)
  (:work 5:50.76 1.00)
  (:work 5:48.38 1.00)
  (:work 5:43.44 1.00)
  (:work 5:34.77 1.00)
  (:work 5:30.27 1.00)
  (:work 5:22.88 1.00)
  (:work 5:24.75 1.00)
  (:work 5:16.99 1.00)
  (:work 5:18.80 1.00))

(defworkout "2011-11-25" "14.52"
  (:warm 3:06.59 .500)
  (:work 5:52.98 1.00)
  (:work 5:48.80 1.00)
  (:work 5:49.52 1.00)
  (:work 5:44.31 1.00)
  (:work 5:35.82 1.00)
  (:work 5:36.48 1.00)
  (:work 5:44.77 1.00)
  (:work 5:46.21 1.00)
  (:work 5:39.27 1.00)
  (:work 5:29.33 1.00))

(defworkout "2011-11-28" "15.33"
  (:warm 5:55.24 1.00)
  (:work 5:40.59 1.00)
  (:work 3:33.41 .642)
  ;pause 3:06.15
  (:hill 0:10.20   40) ;53.6
  (:rest 1:54.00    0)
  (:hill 0:09.77   40) ;53.1
  (:rest 1:56.10    0)
  (:hill 0:09.75   40) ;49.2
  (:rest 1:49.56    0)
  (:hill 0:09.47   40) ;43.2
  (:rest 1:54.38    0)
  (:hill 0:09.53   40) ;48.8
  (:rest 1:56.54    0)
  (:hill 0:09.31   40) ;40.8
  (:rest 1:52.67    0)
  (:hill 0:09.15   40) ;37.6
  (:rest 2:00.10    0)
  (:hill 0:08.10   40) ;34.8
  ;pause 2:21.73
  (:work 5:33.35 1.00)
  (:work 4:04.25 .692))

(defworkout "2011-11-30" "10.52"
  (:warm 5:56.67 1.00)
  (:work 5:49.43 1.00)
  (:work 5:54.27 1.00)
  (:work 5:44.88 1.00)
  (:work 5:40.89 1.00)
  (:work 5:19.89 1.00)
  (:work 5:27.92 1.00)
  (:work 3:59.72 .729)
  ;pause 3:15.47
  (:work 5:41.03 1.00)
  (:work 5:45.80 1.00)
  (:work 2:18.17 .401))

(defworkout "2011-12-28" "16.29"
  (:warm 6:15.33 1.00)
  (:work 5:57.23 1.00)
  (:work 5:52.76 1.00)
  (:work 5:38.19 1.00)
  (:work 2:52.23 .501))

(defworkout "2011-12-30" "16.26"
  (:warm 6:12.41 1.00)
  (:work 5:46.52 1.00)
  (:work 5:48.68 1.00)
  (:work 5:48.31 1.00)
  (:work 5:19.91 1.00)
  (:work 5:45.92 1.00))

(defworkout "2012-01-01" "12.22"
  (:warm 3:09.09 .50)
  (:work 5:49.76 1.00)
  (:work 5:47.28 1.00)
  (:work 5:47.33 1.00)
  (:work 5:41.67 1.00))

(defworkout "2012-01-03" "10.03"
  (:warm 6:10.66 1.00)
  (:work 5:51.39 1.00)
  (:work 5:48.44 1.00)
  (:work 5:25.32 1.00)
  (:work 5:14.20 1.00)
  (:work 5:01.97 .899))

(defworkout "2012-01-05" "17.08"
  (:warm 6:06.27 1.00)
  (:work 5:46.47 1.00)
  (:work 5:45.33 1.00)
  (:work 5:41.01 1.00)
  (:work 5:33.17 1.00)
  (:work 5:36.76 1.00)
  (:work 5:32.59 1.00))

(defworkout "2012-01-14" "15.38"
  (:warm 5:59.13 1.00)
  (:work 5:44.31 1.00)
  (:work 5:36.57 1.00)
  (:work 5:21.46 1.00)
  (:work 2:44.70 .501))

(defworkout "2012-01-16" "16.52"
  (:warm 6:04.90 1.00)
  (:work 5:54.00 1.00)
  (:work 5:45.10 1.00)
  (:work 5:40.82 1.00)
  (:work 5:39.28 1.00)
  (:work 5:37.52 1.00))

(defworkout "2012-01-20" "14.51"
  (:warm 6:02.9 1000)
  (:work 5:45.5 1000)
  ;pause 1:59.9
  (:work 2:05.9  400)
  (:rest 0:59.8    0)
  (:work 2:03.0  400)
  (:rest 0:59.3    0)
  (:work 1:57.4  400)
  (:rest 0:59.4    0)
  (:work 1:56.5  400)
  (:rest 1:01.2    0)
  (:work 1:50.0  400)
  ;pause 4:58.3
  (:work 0:33.6  100)
  (:rest 0:53.6    0)
  (:work 0:31.7  100)
  (:rest 0:57.2    0)
  (:work 0:29.1  100)
  (:rest 0:55.9    0)
  (:work 0:26.2  100)
  (:rest 0:58.8    0)
  (:work 0:24.6  100))

(defworkout "2012-01-23" "16.03"
  (:warm 5:58.42 1.00)
  (:work 5:43.43 1.00)
  (:work 2:22.57 .418)
  ;pause 2:08.65
  (:hill 0:13.35   40)
  (:rest 1:01.55    0)
  (:hill 0:12.20   40)
  (:rest 1:00.80    0)
  (:hill 0:11.23   40)
  (:rest 1:01.60    0)
  (:hill 0:10.87   40)
  (:rest 1:01.20    0)
  (:hill 0:10.20   40)
  (:rest 1:02.33    0)
  (:hill 0:09.60   40)
  (:rest 1:17.60    0)
  (:hill 0:09.69   40)
  (:rest 1:00.58    0)
  (:hill 0:09.78   40)
  ;pause 1:58.52
  (:work 5:49.22 1.00)
  (:work 2:53.31 .50))

(defworkout "2012-01-27" "16.06"
  (:warm 5:54.70 1.00)
  (:work 5:43.05 1.00)
  (:work 5:35.61 1.00)
  (:work 5:40.15 1.00)
  (:work 5:24.02 1.00)
  (:work 4:13.81 .813)
  (:cool 1:39.60 .257))

(defworkout "2012-02-01" "14.59"
  (:warm 5:47.70 1.00)
  (:work 5:40.89 1.00)
  (:work 3:50.80 .701)
  ;pause 2:59.48
  (:hill 0:10.35   40) ;43.9
  (:rest 1:08.87    0)
  (:hill 0:10.13   40) ;42.3
  (:rest 1:01.10    0)
  (:hill 0:09.85   40) ;44.8
  (:rest 1:11.70    0)
  (:hill 0:09.62   40) ;40.7
  (:rest 1:08.15    0)
  (:hill 0:09.33   40) ;37.7
  (:rest 1:12.05    0)
  (:hill 0:09.10   40) ;40.5
  (:rest 1:12.97    0)
  (:hill 0:08.77   40) ;41.9
  (:rest 1:19.83    0)
  (:hill 0:08.40   40) ;41.5
  ;pause 2:15.38
  (:work 5:36.46 1.00)
  (:work 2:34.31 .50))

(defworkout "2012-02-03" "16.35"
  (:warm 6:00.13 1.00)
  (:work 5:52.99 1.00)
  (:work 5:45.98 1.00)
  (:work 5:41.87 1.00)
  (:work 5:37.80 1.00)
  (:work 5:46.69 1.00)
  (:work 5:18.39 1.00))

(defworkout "2012-02-07" "16.19"
  (:warm 5:57.72 1.00)
  (:work 5:42.22 1.00)
  (:work 3:30.75 .602)
  ;pause 2:59.83
  (:hill 0:10.60   40) ;41.9
  (:rest 1:11.95    0)
  (:hill 0:09.60   40) ;42.0
  (:rest 1:13.77    0)
  (:hill 0:09.05   40) ;39.1
  (:rest 1:13.60    0)
  (:hill 0:09.18   40) ;31.4
  (:rest 1:11.70    0)
  (:hill 0:08.80   40) ;40.5
  (:rest 1:20.12    0)
  (:hill 0:08.71   40) ;38.2
  (:rest 1:19.49    0)
  (:hill 0:08.58   40) ;34.6
  (:rest 1:21.35    0)
  (:hill 0:08.62   40) ;34.3
  ;pause 2:29.28
  (:work 5:43.37 1.00)
  (:work 2:51.40 .500))

(defworkout "2012-02-09" "9.55"
  (:warm 6:10.07 1.00)
  (:work 5:45.67 1.00)
  (:work 5:46.16 1.00)
  (:work 5:37.35 1.00)
  (:work 5:20.57 1.00))

(defworkout "2012-02-13" "15.57"
  (:warm 5:58.57 1.00)
  (:work 5:45.47 1.00)
  (:work 3:25.43 .600)
  ;pause 4:01.37
  (:hill 0:08.90   40) ;35.9
  (:rest 1:18.93    0)
  (:hill 0:09.20   40) ;38.4
  (:rest 1:18.90    0)
  (:hill 0:08.69   40) ;36.4
  (:rest 1:18.08    0)
  (:hill 0:08.83   40) ;33.7
  (:rest 1:20.17    0)
  (:hill 0:08.58   40) ;37.4
  (:rest 1:19.70    0)
  (:hill 0:08.85   40) ;31.4
  (:rest 1:21.15    0)
  (:hill 0:08.42   40) ;34.2
  (:rest 1:23.90    0)
  (:hill 0:08.15   40) ;37.5
  ;pause 2:27.05
  (:work 5:51.67 1.00)
  (:work 2:50.06 .502))

(defworkout "2012-02-16" "16.24"
  (:warm 3:07.57  .50)
  (:warm 3:04.42  .50)
  (:work 5:47.45 1.00)
  (:work 5:31.00 1.00)
  (:work 5:36.10 1.00)
  (:work 5:33.37 1.00)
  (:work 5:10.75 1.00))

(defworkout "2012-02-18" "13.21"
  (:warm 6:00.31 1.00)
  (:work 5:43.50 1.00)
  (:work 5:17.14 1.00)
  (:work 5:30.61 1.00)
  (:work 5:18.30 1.00)
  (:work 2:17.58  .40))

(defworkout "2012-02-20" "16.09"
  (:warm 6:00.21 1.00)
  (:work 5:41.16 1.00)
  (:work 3:16.12 .602)
  ;pause 3:37.90
  (:hill 0:09.45   40) ;40.1
  (:rest 1:01.10    0)
  (:hill 0:09.43   40) ;39.4
  (:rest 1:01.20    0)
  (:hill 0:09.70   40) ;38.4
  (:rest 1:00.22    0)
  (:hill 0:09.10   40) ;42.5
  (:rest 1:01.70    0)
  (:hill 0:09.13   40) ;37.3
  (:rest 5:00.57    0)
  (:hill 0:09.00   40) ;42.5
  (:rest 1:01.73    0)
  (:hill 0:09.00   40) ;43.1
  (:rest 1:03.22    0)
  (:hill 0:09.30   40) ;39.5
  (:rest 1:01.38    0)
  (:hill 0:09.27   40) ;40.5
  (:rest 1:06.58    0)
  (:hill 0:09.12   40) ;43.9
  ;pause 3:42.25
  (:work 5:51.85 1.00)
  (:work 2:55.20  .50))

(defworkout "2012-02-22" "9.40"
  (:warm 6:14.97 1.00)
  (:work 5:53.62 1.00)
  (:work 5:46.62 1.00)
  (:work 5:44.69 1.00)
  (:work 5:36.17 1.00)
  (:work 5:26.21 1.00)
  (:work 5:14.92 1.00)
  (:work 5:42.74 1.00))

(defworkout "2012-02-25" "13.01"
  (:warm 6:01.99 1.00)
  (:work 5:42.68 1.00)
  (:work 5:32.61 1.00)
  (:work 5:29.90 1.00)
  (:work 5:32.82 1.00)
  (:work 2:40.22 .500))

(defworkout "2012-02-27" "14.58"
  (:warm 5:52.05 1.00)
  (:work 5:36.67 1.00)
  (:work 3:29.78 .602)
  ;pause 5:03.52
  (:hill 0:08.30   40) ;33.8
  (:rest 2:28.67    0)
  (:hill 0:08.28   40) ;39.0
  (:rest 2:28.22    0)
  (:hill 0:08.30   40) ;42.9
  (:rest 2:27.92    0)
  (:hill 0:08.13   40) ;46.5
  (:rest 2:32.33    0)
  (:hill 0:07.95   40) ;44.8
  ;pause 2:28.80
  (:work 5:44.63 1.00)
  (:work 2:50.54 .500))

(defworkout "2012-02-29" "15.25"
  (:warm 5:46.95 1.00)
  (:work 5:38.78 1.00)
  (:work 5:31.81 1.00)
  (:work 5:22.14 1.00)
  (:work 4:10.61 .802)
  ;pause 9:42.08
  (:work 5:19.23 1.00)
  (:work 1:07.57 .204))

(defworkout "2012-03-02" "16.05"
  (:warm 6:04.6 1000)
  (:warm 5:37.6 1000)
  ;pause 5:18.7
  (:work 1:54.4  400)
  (:rest 0:59.9    0)
  (:work 1:54.0  400)
  (:rest 0:58.7    0)
  (:work 1:52.4  400)
  (:rest 0:59.9    0)
  (:work 1:51.6  400)
  (:rest 0:58.7    0)
  (:work 1:48.4  400)
  (:rest 1:01.0    0)
  (:work 1:48.7  400)
  (:rest 0:59.4    0)
  (:work 1:44.8  400)
  (:rest 1:02.1    0)
  (:work 1:45.0  400)
  (:rest 1:00.4    0)
  (:work 1:44.2  400)
  (:rest 0:59.2    0)
  (:work 1:41.3  400))

(defworkout "2012-03-05" "16.07"
  (:warm 5:57.73 1.00)
  (:work 5:35.93 1.00)
  (:work 3:26.58 .604)
  ;pause 4:51.73
  (:hill 0:09.30   40) ;46.4
  (:rest 1:15.82    0)
  (:hill 0:08.85   40) ;43.3
  (:rest 1:12.75    0)
  (:hill 0:08.98   40) ;45.3
  (:rest 1:11.55    0)
  (:hill 0:08.90   40) ;41.6
  (:rest 1:13.35    0)
  (:hill 0:08.60   40) ;40.8
  (:rest 4:46.05    0)
  (:hill 0:08.67   40) ;41.9
  (:rest 1:11.53    0)
  (:hill 0:08.65   40) ;44.1
  (:rest 1:11.62    0)
  (:hill 0:08.75   40) ;44.4
  (:rest 1:15.13    0)
  (:hill 0:08.75   40) ;45.1
  (:rest 1:14.80    0)
  (:hill 0:08.02   40) ;38.6
  ;pause 3:17.00
  (:work 5:35.08 1.00)
  (:work 2:50.35 .502))

(defworkout "2012-03-07" "10.12"
  (:warm 6:07.56 1.00)
  (:work 5:49.37 1.00)
  (:work 5:48.50 1.00)
  (:work 5:46.06 1.00)
  (:work 5:38.83 1.00)
  (:work 5:38.92 1.00)
  (:work 5:26.65 1.00)
  (:work 5:27.49 1.00)
  (:work 5:28.17 1.00))

(defworkout "2012-03-09" "15.46"
  (:warm 5:58.5 1000)
  (:warm 5:36.0 1000)
  ;pause 5:59.6
  (:work 1:55.7  400)
  (:rest 1:00.0    0)
  (:work 1:53.3  400)
  (:rest 1:00.3    0)
  (:work 1:49.9  400)
  (:rest 0:59.6    0)
  (:work 1:49.3  400)
  (:rest 0:59.8    0)
  (:work 1:44.9  400)
  (:rest 1:00.2    0)
  (:work 1:45.2  400)
  (:rest 0:59.5    0)
  (:work 1:44.1  400)
  (:rest 1:00.6    0)
  (:work 1:42.0  400)
  (:rest 1:00.8    0)
  (:work 1:42.4  400)
  (:rest 1:02.2    0)
  (:work 1:35.0  400))

(defworkout "2012-03-12" "15.53"
  (:warm 5:51.14 1.00)
  (:work 5:30.94 1.00)
  (:work 3:24.78 .61)
  ;pause 2:28.82
  (:warm 0:10.03   40) ;51.8
  ;pause 4:44.60
  (:hill 0:08.50   40) ;50.6
  (:rest 2:32.05    0)
  (:hill 0:08.02   40) ;42.4
  (:rest 2:27.68    0)
  (:hill 0:08.00   40) ;41.5
  (:rest 2:26.95    0)
  (:hill 0:08.47   40) ;49.8
  (:rest 2:33.90    0)
  (:hill 0:08.23   40) ;40.6
  ;pause 2:52.00
  (:work 5:45.99 1.00)
  (:work 2:52.98 .50))

(defworkout "2012-03-14" "10.46"
  (:warm 6:00.19 1.00)
  (:warm 1:07.70 .199)
  (:warm 5:32.05 1000)
  ;pause 1:17.65
  (:work 5:08.33 1000)	;55
  (:rest 1:01.07    0)
  (:work 4:58.60 1000)	;46
  (:rest 0:59.75    0)
  (:work 4:55    1000)	;80	 ;4:32.72 + 0:31.88 / 1034.8 meter
  (:rest 0:59.85    0)
  (:work 4:54.18 1000)	;36
  (:rest 0:59.55    0)
  (:work 4:35.25 1000)	;67
  ;pause 3:14.52
  (:cool 5:25.72 1.00))

(defworkout "2012-03-16" "15.03"
  (:warm 6:02.7 1000)
  (:warm 5:36.6 1000)
  (:warm 2:06.7  400)
  ;pause 4:05.4
  (:work 1:55.8  400)
  (:rest 0:59.4    0)
  (:work 1:49.7  400)
  (:rest 0:59.6    0)
  (:work 1:47.5  400)
  (:rest 0:59.3    0)
  (:work 1:47.0  400)
  (:rest 0:58.5    0)
  (:work 1:44.2  400)
  (:rest 0:58.9    0)
  (:work 1:43.5  400)
  (:rest 0:59.5    0)
  (:work 1:39.9  400)
  (:rest 1:00.5    0)
  (:work 1:40.3  400)
  (:rest 1:01.5    0)
  (:work 1:36.7  400)
  (:rest 1:01.2    0)
  (:work 1:31.6  400))

(defworkout "2012-03-19" "15.15"
  (:warm 6:02.85 1.00)
  (:work 5:39.80 1.00)
  ;pause 3:01.87
  (:hill 0:09.51   40) ;39.5
  (:rest 1:42.29    0)
  (:hill 0:09.77   40) ;49.9
  (:rest 1:35.65    0)
  (:hill 0:09.80   40) ;50.7
  (:rest 1:29.20    0)
  (:hill 0:09.78   40) ;47.6
  (:rest 1:35.97    0)
  (:hill 0:09.23   40) ;46.6
  (:rest 1:30.90    0)
  (:hill 0:09.42   40) ;53.6
  ;pause 2:48.42
  (:work 5:36.25 1.00)
  (:work 2:49.10 .513))

(defworkout "2012-03-22" "9.41"
  (:warm 6:05.2 1.00)
  (:warm 1:11.0 0.20)
  (:warm 5:35.2 1000)
  ;pause 3:19.8
  (:work 1:20.4  300)
  (:rest 0:59.0    0)
  (:work 1:19.2  300)
  (:rest 0:59.8    0)
  (:work 1:17.8  300)
  (:rest 0:58.6    0)
  (:work 1:16.5  300)
  (:rest 0:59.0    0)
  (:work 1:15.1  300)
  (:rest 0:59.9    0)
  (:work 1:13.1  300)
  (:rest 0:56.5    0)
  (:work 1:12.1  300)
  (:rest 0:59.0    0)
  (:work 1:11.1  300)
  (:rest 0:57.6    0)
  (:work 1:09.1  300)
  (:rest 1:03.3    0)
  (:work 0:59.3  300)
  ;pause 3:17.7
  (:cool 5:40.3 1.00))

(defworkout "2012-03-25" "8.47"
  (:warm 2:59.84 .500)
  (:work 5:30.91 1.00)
  (:work 5:25.19 1.00)
  (:work 5:08.93 1.00)
  (:work 5:00.67 1.00)
  (:work 4:46.85 1.00)
  (:cool 2:44.68 .505))

(defworkout "2012-03-27" "18.02"
  (:warm 5:57.8 1.00)
  (:warm 5:40.2 1.00)
  (:warm 1:54.2 .333)
  ;pause 1:41.4
  (:warm 0:22.2  100)
  ;pause 7:05.3
  (:work 3:44.6  800)
  (:rest 1:11.6 .16)
  (:work 3:45.4  800)
  (:rest 1:05.5 .16)
  (:work 3:45.9  800)
  (:rest 1:07.0 .16)
  (:work 3:42.1  800)
  (:rest 1:23.3 .16)
  (:work 3:38.4  800)
  (:rest 1:10.2 .16)
  (:work 3:32.2  800)
  ;pause 3:04.9
  (:cool 5:50.2 1.00))

(defworkout "2012-03-29" "10.56"
  (:warm 3:01.4 .500)
  (:work 2:53.5 .500)
  (:rest 1:22.6    0)
  (:work 3:03.5 .500))

(defworkout "2012-03-30" "14.53"
  (:warm 5:51.17 1.00)
  (:work 5:30.41 1.00)
  (:work 3:15.67 .601)
  ;pause 4:33.85
  (:hill 0:08.80   40)
  (:rest 1:14.30    0)
  (:hill 0:08.83   40)
  (:rest 1:15.50    0)
  (:hill 0:08.95   40)
  (:rest 1:13.90    0)
  (:hill 0:08.87   40)
  (:rest 1:15.90    0)
  (:hill 0:08.85   40)
  (:rest 5:59.53    0)
  (:hill 0:08.90   40)
  (:rest 1:17.70    0)
  (:hill 0:09.05   40)
  (:rest 1:18.48    0)
  (:hill 0:08.72   40)
  (:rest 1:17.65    0)
  (:hill 0:08.82   40)
  (:rest 1:29.20    0)
  (:hill 0:08.43   40)
  ;pause 2:25.54
  (:work 5:37.62 1.00)
  (:work 2:53.14 .502))

(defworkout "2012-04-02" "11.24"
  (:warm 5:49.8 1.00)
  (:warm 1:11.6 0.20)
  (:warm 5:18.3 1000)
  ;pause 1:58.0
  (:warm 0:47.4  200)
  (:rest 1:58.4    0)
  (:warm 0:40.9  200)
  (:rest 1:55.5    0)
  (:warm 0:37.4  200)
  ;pause 6:36.0
  (:work 1:39.4  400)
  (:rest 0:59.6    0)
  (:work 1:38.2  400)
  (:rest 1:01.7    0)
  (:work 1:35.0  400)
  (:rest 0:59.3    0)
  (:work 1:35.9  400)
  (:rest 1:00.2    0)
  (:work 1:34.7  400)
  (:rest 6:08.7    0)
  (:work 1:39.9  400)
  (:rest 0:58.6    0)
  (:work 1:36.0  400)
  (:rest 0:59.4    0)
  (:work 1:36.3  400)
  (:rest 1:00.1    0)
  (:work 1:35.5  400)
  (:rest 1:00.6    0)
  (:work 1:31.1  400)
  ;pause 8:54.6
  (:cool 5:44.9 1.00))

(defworkout "2012-04-04" "15.04"
  (:warm 5:54.87 1.00)
  (:work 5:35.68 1.00)
  (:work 5:25.14 1.00)
  (:work 5:16.33 1.00)
  (:work 5:26.19 1.00)
  (:work 5:38.24 1.00)
  (:work 5:23.77 1.00)
  (:work 5:24.72 1.00)
  (:work 5:30.66 1.00))

(defworkout "2012-04-06" "11.04"
  (:warm 5:53.6 1000)
  (:warm 5:27.6 1000)
  ;pause 2:31.5
  (:work 4:56.5 1000)
  (:rest 0:58.2    0)
  (:work 4:55.4 1000)
  (:rest 0:58.9    0)
  (:work 4:50.2 1000)
  (:rest 0:59.3    0)
  (:work 4:49.0 1000)
  (:rest 0:59.4    0)
  (:work 4:46.9 1000)
  (:rest 0:59.3    0)
  (:work 4:40.7 1000))

(defworkout "2012-04-08" "13.57"
  (:warm 5:53.3 1000)
  (:warm 5:29.1 1000)
  (:warm 5:04.5 1000)
  ;pause 2:16.2
  (:warm 0:21.0  100)
  (:rest 1:16.1    0)
  (:warm 0:20.8  100)
  (:rest 1:16.6    0)
  (:warm 0:19.2  100)
  ;pause 4:00.4
  (:work 0:45.5  200)
  (:rest 0:59.9    0)
  (:work 0:44.7  200)
  (:rest 0:58.0    0)
  (:work 0:43.2  200)
  (:rest 1:00.1    0)
  (:work 0:42.0  200)
  (:rest 0:58.1    0)
  (:work 0:42.1  200)
  (:rest 0:59.1    0)
  (:work 0:40.1  200)
  (:rest 2:57.7    0)
  (:work 0:43.1  200)
  (:rest 0:59.9    0)
  (:work 0:40.9  200)
  (:rest 0:58.8    0)
  (:work 0:40.6  200)
  (:rest 1:00.5    0)
  (:work 0:40.0  200)
  (:rest 1:00.2    0)
  (:work 0:40.9  200)
  (:rest 0:59.2    0)
  (:work 0:37.4  200)
  ;pause 3:37.5
  (:cool 1:46.6  300))

(defworkout "2012-04-16" "15.57"
  (:warm 6:03.65 1.00)
  (:work 5:47.39 1.00)
  (:work 3:21.85 .601)
  ;pause 3:26.40
  (:hill 0:09.60   40)
  (:rest 1:55.78    0)
  (:hill 0:09.20   40)
  (:rest 1:58.67    0)
  (:hill 0:08.95   40)
  (:rest 1:58.55    0)
  (:hill 0:08.70   40)
  (:rest 1:59.78    0)
  (:hill 0:08.80   40)
  (:rest 2:00.32    0)
  (:hill 0:08.58   40)
  ;pause 2:31.05
  (:work 5:47.72 1.00)
  (:work 3:24.48 .600))

(defworkout "2012-04-18" "15.39"
  (:warm 5:51.83 1.00)
  (:warm 5:37.48 1.00)
  (:warm 5:29.97 1.00)
  (:work 5:13.79 1.00)
  (:work 5:07.83 1.00)
  (:work 2:25.01 .502)
  (:cool 2:55.09 .502))

(defworkout "2012-04-21" "13.46"
  (:warm 6:10.2 1000)
  (:warm 5:37.0 1000)
  (:warm 5:08.0 1000)
  ;pause 12:37.0
  (:warm 0:16.7  100)
  (:rest 1:56.3    0)
  (:warm 0:15.8  100)
  (:rest 1:53.5    0)
  (:warm 0:15.0  100)
  ;pause 7:09.7
  (:work 0:43.5  200)
  (:rest 0:55.8    0)
  (:work 0:40.0  200)
  (:rest 0:56.4    0)
  (:work 0:38.3  200)
  (:rest 1:00.3    0)
  (:work 0:38.0  200)
  (:rest 1:00.5    0)
  (:work 0:39.2  200)
  (:rest 1:00.2    0)
  (:work 0:36.9  200)
  (:rest 8:10.2    0)
  (:work 0:40.8  200)
  (:rest 0:59.1    0)
  (:work 0:41.2  200)
  (:rest 1:00.3    0)
  (:work 0:39.8  200)
  (:rest 0:59.4    0)
  (:work 0:39.0  200)
  (:rest 1:00.4    0)
  (:work 0:38.9  200)
  (:rest 1:01.4    0)
  (:work 0:35.7  200)
  ;pause 6:25.6
  (:cool 1:47.7  300))

(defworkout "2012-04-23" "15.45"
  (:warm 6:00 1000)
  (:work 5:30 1000)
  (:rest 2:00    0)
  (:work 0:18  100)
  (:rest 2:00    0)
  (:work 0:18  100)
  (:rest 2:00    0)
  (:work 0:18  100))

;(defworkout 2012-04-25
;  800m @ 2:43

(defworkout "2012-04-28" "14.47"
  (:warm 6:15.6 1000)
  (:warm 5:36.3 1000)
  ;pause 1:01.1
  (:warm 0:53.0  200)
  (:rest 0:59.4    0)
  (:warm 0:46.1  200)
  (:rest 0:59.8    0)
  (:warm 0:40.1  200)
  (:rest 0:59.7    0)
  (:warm 0:36.8  200)
  ;pause 10:51.7
  (:work 0:50.9  300)
  (:rest 9:57.8    0)
  (:work 0:48.9  300)
  (:rest 12:54.1   0)
  (:work 0:47.7  300))

(defworkout "2012-05-02" "18.00"
  (:warm 5:00   800)
  (:warm 0:21.4 100)
  (:warm 0:16.6 100)
  (:warm 0:14.6 100)
  ;; ^^TRAINERS  vvFLATS
  (:warm 0:15.4 100)
  (:warm 0:14.3 100)
  (:warm 0:13.8 100)
  (:warm 0:15.0 100)
  (:warm 0:14.1 100)
  ;; 20:10
  (:work 0:26.8 200))

(defworkout "2012-05-20" "15.19"
  (:warm 5:53.46 1.00)
  (:work 5:49.84 1.00)
  (:work 5:32.46 1.00)
  (:work 5:23.72 1.00)
  (:work 3:37.84 0.70)
  ;pause 11:17.33
  (:work 5:36.88 1.00)
  (:cool 1:50.60 .311))

(defworkout "2012-05-25" "10.12"
  (:warm 6:14.22 1.00)
  (:work 5:45.89 1.00)
  (:work 5:38.99 1.00)
  (:work 5:37.23 1.00)
  (:work 5:37.59 1.00))

(defworkout "2012-05-28" "10.12"
  (:warm 6:01.16 1.00)
  (:work 5:41.41 1.00)
  (:work 5:36.12 1.00)
  (:work 5:31.69 1.00)
  (:work 0:45.84 .138)
  ;pause 2:22.05
  (:work 5:29.18 1.00))

(defworkout "2012-05-30" "10.39"
  (:warm 5:56.75 1.00)
  (:work 5:52.17 1.00)
  (:work 5:37.27 1.00)
  (:work 5:35.42 1.00)
  (:work 5:12.46 1.00)
  (:cool 1:12.90 .202))

(defworkout "2012-06-03" "11.05"
  (:warm 6:07.82 1.00)
  (:work 5:50.92 1.00)
  (:work 5:36.57 1.00)
  (:work 5:19.90 1.00)
  (:work 5:36.11 1.00)
  (:work 5:08.34 1.00))

(defworkout "2012-06-06" "17.00"
  (:warm 6:37.18 1.00)
  (:rest 2:46.54    0)
  (:work 6:02.87 1.00)
  (:rest 2:32.63    0)
  (:work 5:44.49 1.00)
  (:rest 2:13.06    0)
  (:work 5:29.21 1.00)
  (:rest 1:57.79    0)
  (:work 5:33.70 1.00)
  (:rest 1:16.10    0)
  (:work 2:52.75 .502))

(defworkout "2012-06-09" "14.33"
  (:warm 6:01.42 1.00)
  (:rest 1:54.85    0)
  (:work 5:42.22 1.00)
  (:rest 2:57.50    0)
  (:work 5:44.68 1.00)
  (:rest 1:51.15    0)
  (:work 6:29.40 1.00)
  (:rest 2:32.38    0)
  (:work 5:33.53 1.00)
  (:rest 4:40.71    0)
  (:work 4:33.65 .759))

(defworkout "2012-06-12" "11.21"
  (:warm 6:02.08 1.00)
  (:rest 2:38.86    0)
  (:work 5:41.04 1.00)
  (:rest 1:57.11    0)
  (:work 5:25.70 1.00)
  (:rest 1:48.05    0)
  (:work 5:18.09 1.00)
  (:rest 1:51.85    0)
  (:work 5:00.34 1.00)
  (:rest 0:27.28    0)
  (:cool 1:41.83 .307))

(defworkout "2012-06-15" "15.03"
  (:warm 6:08.16 1.00)
  (:work 4:07.98 .702)
  ;pause 4:24.55
  (:hill 0:12.08   40)
  (:rest 1:03.70    0)
  (:hill 0:10.60   40)
  (:rest 1:12.20    0)
  (:hill 0:09.90   40)
  (:rest 1:20.87    0)
  (:hill 0:09.85   40)
  (:rest 1:18.68    0)
  (:hill 0:09.90   40)
  (:rest 1:29.25    0)
  (:hill 0:09.67   40)
  (:rest 1:17.98    0)
  (:hill 0:09.15   40)
  (:rest 1:15.90    0)
  (:hill 0:09.10   40)
  (:rest 1:14.70    0)
  (:hill 0:09.42   40)
  (:rest 1:15.30    0)
  (:hill 0:09.10   40)
  ;pause 2:02.45
  (:work 5:25.77 1.00)
  (:work 2:26.28 .501))

(defworkout "2012-06-18" "10.27"
  (:warm 3:09.52 .50) ;776
  (:warm 2:59.67 .50) ;352
  (:rest 1:50.90    0)
  (:work 5:37.72 1.00)
  (:rest 2:25.00    0)
  (:work 5:21.18 1.00)
  (:rest 1:48.15    0)
  (:work 3:53.85 .699))

(defworkout "2012-06-21" "15.26"
  (:warm 3:03.62 .50) ;522
  (:work 5:46.41 1.00)
  (:work 5:34.63 1.00)
  (:work 5:35.49 1.00)
  (:work 5:30.58 1.00)
  (:work 5:28.03 1.00)
  (:cool 2:58.36 .504))

(defworkout "2012-06-25" "16.15"
  (:warm 6:01.10 1.00)
  (:work 5:44.11 1.00)
  (:work 5:35.53 1.00)
  (:work 5:26.74 1.00)
  (:work 5:16.98 1.00)
  (:work 5:34.83 1.00))

(defworkout "2012-06-28" "16.14"
  (:warm 6:00    1.00)
  (:work 5:42.67 1.00)
  (:work 5:33.69 1.00)
  (:work 5:26.01 1.00)
  (:work 5:31.62 1.00)
  (:work 5:33.43 1.00))

(defworkout "2012-07-02" "16.05"
  (:warm 6:01.67 1.00)
  (:work 5:37.52 1.00)
  ;pause 5:49.34
  (:hill 0:09.57   40)
  (:rest 1:29.15    0)
  (:hill 0:09.08   40)
  (:rest 1:26.15    0)
  (:hill 0:09.17   40)
  (:rest 1:26.95    0)
  (:hill 0:09.40   40)
  (:rest 1:28.70    0)
  (:hill 0:08.73   40)
  (:rest 1:32.10    0)
  (:hill 0:08.95   40)
  (:rest 1:27.50    0)
  (:hill 0:08.90   40)
  (:rest 1:30.95    0)
  (:hill 0:08.90   40)
  (:rest 1:31.92    0)
  (:hill 0:08.55   40)
  (:rest 1:29.55    0)
  (:hill 0:08.15   40)
  ;pause 2:53.40
  (:work 5:37.30 1.00)
  (:work 3:25.85 .601))

(defworkout "2012-07-06" "10.57"
  (:warm 5:55.29 1.00)
  (:work 5:51.92 1.00)
  (:work 5:38.77 1.00)
  (:work 5:29.95 1.00)
  ;pause 5:15.41
  (:work 5:11.04 1.00)
  (:work 1:39.70 .301))

(defworkout "2012-07-09" "10.47"
  (:warm 3:00.32 .50)
  (:work 5:54.27 1.00)
  (:work 5:33.89 1.00)
  (:work 5:32.88 1.00)
  (:work 5:38.30 1.00)
  (:work 5:31.99 1.00)
  (:work 5:39.63 1.00)
  (:work 1:47.61 .298))

(defworkout "2012-07-12" "16.26"
  (:warm 3:00.02 .50)
  (:warm 5:53.52 1.00)
  (:work 2:38.55 .505)
  ;pause 4:56.93
  (:hill 0:10.56   40)
  (:rest 1:29.27    0)
  (:hill 0:09.60   40)
  (:rest 1:31.78    0)
  (:hill 0:09.52   40)
  (:rest 1:30.40    0)
  (:hill 0:09.28   40)
  (:rest 1:30.62    0)
  (:hill 0:09.13   40)
  (:rest 1:29.40    0)
  (:hill 0:09.25   40)
  (:rest 1:29.47    0)
  (:hill 0:09.35   40)
  (:rest 1:31.13    0)
  (:hill 0:08.37   40)
  ;pause 3:23.83
  (:work 5:38.25 1.00)
  (:work 2:51.65 .50))

(defworkout "2012-07-15" "14.57"
  (:warm 6:10.12 1.00)
  (:work 5:54.59 1.00)
  (:work 5:46.85 1.00)
  (:work 4:32.36 .875)
  ;pause 2:15.52
  (:work 5:08.09 1.00)
  (:work 3:33.56 .700)
  (:cool 3:02.08 .500))

(defworkout "2012-07-19" "16.11"
  (:warm 0:06:07.47 1.00)
  (:work 0:05:36.10 1.00)
  ;pause 0:06:30.99
  (:hill 0:00:09.32   40) ;64.1
  (:rest 0:01:39.23    0)
  (:hill 0:00:09.20   40) ;38.7
  (:rest 0:01:39.67    0)
  (:hill 0:00:08.93   40) ;46.3
  (:rest 0:01:40.07    0)
  (:hill 0:00:08.66   40) ;47.5
  (:rest 0:01:40.09    0)
  (:hill 0:00:08.60   40) ;48.9
  (:rest 0:01:38.73    0)
  (:hill 0:00:08.25   40) ;45.3
  (:rest 0:01:39.62    0)
  (:hill 0:00:08.52   40) ;51.0
  (:rest 0:01:38.81    0)
  (:hill 0:00:08.27   40) ;56.1
  (:rest 0:01:39.45    0)
  (:hill 0:00:08.61   40) ;40.2
  (:rest 0:01:42.47    0)
  (:hill 0:00:08.37   40) ;47.6
  ;pause 0:03:43.40
  (:work 0:05:35.23 1.00)
  (:work 0:02:54.30  .50))

(defworkout "2012-07-22" "14.52"
  (:warm 6:10.99 1.00)
  (:work 5:57.04 1.00)
  (:work 5:47.05 1.00)
  (:work 5:39.90 1.00)
  (:work 5:37.55 1.00)
  (:work 5:46.16 1.00)
  (:work 5:39.13 1.00))

(defworkout "2012-07-25" "11.08"
  (:warm 6:12.86 1.00)
  (:work 5:48.44 1.00)
  (:work 5:43.98 1.00)
  (:work 5:51.56 1.00)
  (:work 5:38.02 1.00)
  (:work 2:57.91 .499))

(defworkout "2012-07-28" "14.24"
  (:warm 6:20.70 1.00)
  (:work 6:29.05 1.00)
  (:work 6:09.93 1.00)
  (:work 5:35.07 1.00)
  (:work 5:47.56 1.00))

(defworkout "2012-07-31" "15.34"
  (:warm 6:02.75 1.00)
  (:work 5:24.32 1.00)
  (:work 0:42.15 .108)
  ;pause 5:31.29
  (:hill 0:09.41   40)
  (:rest 1:37.97    0)
  (:hill 0:08.90   40)
  (:rest 1:36.55    0)
  (:hill 0:08.60   40)
  (:rest 1:39.75    0)
  (:hill 0:08.73   40)
  (:rest 1:39.50    0)
  (:hill 0:08.92   40)
  (:rest 2:59.33    0)
  (:hill 0:08.87   40)
  (:rest 1:38.35    0)
  (:hill 0:08.82   40)
  (:rest 1:37.68    0)
  (:hill 0:08.68   40)
  (:rest 1:40.10    0)
  (:hill 0:08.72   40)
  (:rest 1:41.58    0)
  (:hill 0:08.05   40)
  ;pause 3:45.35
  (:work 5:36.06 1.00)
  (:work 2:40.74 .500))

(defworkout "2012-08-03" "16.26"
  (:warm 6:25.43 1.00)
  (:work 5:53.30 1.00)
  (:work 5:51.72 1.00)
  (:work 5:39.15 1.00)
  (:work 5:48.53 1.00)
  (:cool 6:14.93 1.00))

(defworkout "2012-09-05" "16.13"
  (:warm 6:02.6 1.00)
  (:work 5:26.6 1.00))

;defworkout "2012-09-06" "" (:work ? 1.2))
;defworkout "2012-09-06" "" (:work ? 1.2))

;defworkout "2012-09-09" "" (:work ? 1.26))

;defworkout "2012-09-10" "" (:work ? 1.2))
;defworkout "2012-09-10" "" (:work ? 2.0))

;defworkout "2012-09-12" "" (:work ? 1.5))

;defworkout "2012-09-13" "" (:work ? 2.0))
;defworkout "2012-09-13" "" (:work ?

(defworkout "2012-09-20" "16.18"
  (:warm 6:09.22 1.00)
  (:work 5:51.12 1.00)
  (:work 5:25.52 1.00)
  (:work 5:47.53 1.00))

(defworkout "2012-09-22" "13.41"
  (:warm 8:18.98 1.00)
  (:work 7:50.02 1.00)
  (:work 0:43.73 .099))

(defworkout "2012-09-23" "11.36"
  (:warm 6:35.26 1.00)
  (:work 6:04.93 1.00)
  (:work 6:06.78 1.00)
  (:work 5:49.92 1.00)
  (:work 2:02.67 .399))

;defworkout "2012-09-24" "" (:work ? 2.0)

(defworkout "2012-09-28" "16.38"
  (:warm 6:21.25 1.00)
  (:work 5:50.91 1.00)
  (:work 5:36.23 1.00)
  (:work 2:45.73 .502)
  ;pause 9:30.32
  (:work 5:14.40 1.00)
  (:work 2:54.60 .498))

(defworkout "2012-09-29" "14.??"
  (:warm 7:58.94 1.00)
  (:warm 0:41.68 .100)
  (:rest 2:44.35    0)
  (:work 8:08.21 1.00)
  (:warm 0:56.04 .119))

(defworkout "2012-09-30" "11.42"
  (:warm 6:27.21 1.00)
  (:work 6:11.47 1.00)
  (:work 6:18.39 1.00)
  (:work 5:59.97 1.00))

(defworkout "2012-10-01" "16.47"
  (:work 3:51.97 .50)
  (:rest 1:12.22 .10)
  (:work 3:48.48 .50)
  (:rest 1:08.52 .96)
  (:work 3:55.45 .50)
  (:rest 1:00.18 .77)
  (:work 3:51.75 .50)
  (:rest 1:07.37 .10)
  (:work 4:13.60 .54))

;;;(defworkout "2012-10-07" 

(defworkout "2012-10-09" "17.02"
  (:work 4:02.01 .50)
  (:rest 1:17.66 .09)
  (:work 4:00    .50)
  (:rest 1:32.77 .09)
  (:work 3:59.15 .500)
  (:rest 1:58.53 .08)
  (:work 4:05.97 .502)
  (:rest 1:18.10 .08)
  (:work 3:58.63 .503)
  (:rest 1:22.32 .06)
  (:work 3:49.73 .503))

(defworkout "2012-10-11" "16.28"
  (:warm 6:13.62 1.00)
  (:work 5:51.87 1.00)
  (:work 5:24.04 1.00)
  (:work 5:10.81 1.00))

(defworkout "2012-10-13" "14.17"
  (:warm 6:11.08 1.00)
  (:work 5:31.57 1.00)
  (:work 5:28.16 1.00)
  (:work 5:31.19 1.00)
  (:work 2:40.92 .502))

;;;(defworkout "2012-10-14"  2.00

(defworkout "2012-10-16" "16.06"
  (:warm 6:18.84 1.00)
  (:work 5:50.93 1.00)
  (:work 5:33.61 1.00)
  (:work 5:18.32 1.00)
  (:cool 6:13.28 1.00))

(defworkout "2012-11-22" "16.34"
  (:warm 6:25.57 1.00)
  (:work 5:56.59 1.00)
  (:work 5:43.29 1.00)
  (:work 5:37.39 1.00)
  (:work 5:32.19 1.00))

(defworkout "2012-12-17" "16.14"
  (:warm 6:23.15 1.00)
  (:warm 6:03.33 1.00)
  (:work 5:49.61 1.00)
  (:work 5:39.61 1.00)
  (:work 5:26.69 1.00))

(defworkout "2012-12-20" "16.00"
  (:warm 6:06.02 1.00)
  (:warm 5:39.43 1.00)
  (:work 5:36.22 1.00)
  (:work 5:21.10 1.00)
  (:cool 4:14.45 .700))

(defworkout "2012-12-27" "16.59"
  (:warm 6:22.63 1.00)
  (:warm 6:00.65 1.00)
  (:work 5:50.63 1.00)
  (:work 5:31.21 1.00)
  (:work 5:20.41 1.00)
  (:work 5:11.71 1.00))

(defworkout "2013-01-01" "15.08"
  (:warm 6:17.79 1.00)
  (:work 5:52.00 1.00)
  (:work 5:37.73 1.00)
  (:work 5:28.12 1.00)
  (:work 5:15.44 1.00)
  (:work 5:48.67 1.00))

(defworkout "2013-01-06" "16.41"
  (:warm 6:07.19 1.00)
  (:work 5:45.16 1.00)
  (:work 5:38.52 1.00)
  (:work 5:25.75 1.00)
  (:work 5:37.29 1.00)
  (:work 5:26.48 1.00)
  (:work 5:46.32 1.00))

(defworkout "2013-01-10" "16.32"
  (:warm 6:05.50 1.00)
  (:work 5:41.33 1.00)
  (:work 5:33.83 1.00))

(defworkout "2013-01-13" "16.37"
  (:warm 5:55.64 1.00)
  (:work 5:35.82 1.00)
  (:work 5:21.92 1.00)
  (:work 5:15.93 1.00)
  (:work 5:09.62 1.00)
  (:work 5:46.56 1.00))

(defworkout "2013-01-20" "15.00"
  (:warm 6:13.94 1.00)
  (:work 5:45.99 1.00)
  (:work 5:37.63 1.00)
  (:work 5:36.17 1.00)
  (:work 5:30.85 1.00)
  (:work 5:38.91 1.00)
  (:work 5:52.39 1.00))

(defworkout "2013-01-27" "15.42"
  (:warm 5:55.20 1.00)
  (:work 5:41.53 1.00)
  (:work 5:19.82 1.00)
  (:work 5:41.63 1.00)
  (:work 5:20.09 1.00)
  (:work 5:39.52 1.00)
  (:work 5:12.55 1.00)
  (:cool 1:51.85 .304))

;defworkout "2013-01-29" "16.54"
;;; work 9:09.62 1.10
;;; rest 2:42.30
;;; work 4:27.37 .548
;;; rest 0:58.58
;;; work 4:17.37 .527

(defworkout "2013-02-03" "15.21"
  (:warm 6:13.79 1.00)
  (:work 5:50.42 1.00)
  (:work 5:39.23 1.00)
  (:work 5:32.90 1.00)
  (:work 5:30.85 1.00)
  (:work 5:41.54 1.00)
  (:work 5:32.80 1.00)
  (:work 5:44.67 1.00))

(defworkout "2013-02-05" "16.45"
  (:warm 6:03.62 1.00)
  (:work 5:45.06 1.00)
  (:work 5:22.89 1.00)
  (:work 5:39.31 1.00))

(defworkout "2013-02-19" "17.01"
  (:warm 5:55.92 1.00)
  (:work 5:34.63 1.00)
  (:work 5:27.33 1.00)
  (:work 5:32.40 1.00)
  (:work 5:27.71 1.00)
  (:cool 5:51.07 1.00))

(defworkout "2013-03-03" "14.03"
  (:warm 5:57.07 1.00)
  (:work 5:48.90 1.00)
  (:work 5:40.18 1.00)
  (:work 5:36.08 1.00)
  (:work 5:29.90 1.00)
  (:work 5:16.01 1.00))

(defworkout "2013-03-10" "15.41"
  (:warm 6:03.24 1.00)
  (:work 5:47.11 1.00)
  (:work 5:40.91 1.00)
  (:work 5:48.25 1.00)
  (:work 5:47.90 1.00)
  (:work 5:50.68 1.00))

(defworkout "2013-03-28" "16.11"
  (:warm 6:15.88 1.00)
  (:warm 6:04.41 1.00)
  (:work 5:49.61 1.00)
  (:work 5:38.62 1.00)
  (:work 5:45.10 1.00)
  (:work 5:47.26 1.00))

#|
kg	m	BMI	BMI5
54	1.56	22.2	5.8
76	1.70	26.3	5.4
70	1.83	20.9	3.4
76	1.83	22.7	3.7
108	1.74	30.9	5.4
|#

#|
http://soundcloud.com/runningeclipse/going-to-spain
http://soundcloud.com/dj-eclipse-4/wifi-tost

http://www.letsrun.com/forum/flat_read.php?thread=3780094&page=4
http://www.coreawareness.com/podcasts/bio-mechanics-bowman/
http://ericcressey.com/the-5-most-common-speed-quickness-and-explosiveness-problems-in-athletes-part-2
http://www.marathonperformance.com/2010/01/an-event-of-intensity-800m-training/
http://www.marathonperformance.com/forum/topic.php?id=35
http://www.skyscrapercity.com/showthread.php?t=705192&page=6
http://www.angeredsis.se/historik.html
|#

#|
(defun (torus-distance float) ((p vec3) (r1 float) (r2 float))
  (- (length (vec2 (- (length (xy p)) r1) (z p))) r2))

(defun (torus-normal vec3) ((p vec3) (r1 float))
  (- p (vec3 (* (xy p) (/ r1 (length (xy p)))) 0)))

(defun (super-sphere-distance ((p vec3) (r float) (k float))
  (let ((pk (expt (abs p) k)))
    (- (expt (+ (x pk) (y pk) (z pk)) (/ k)) r))))

(defun (super-sphere-normal ((p vec3) (r float) (k float))
  (let ((pk (expt (abs p) k)))
    (/ pk p))))
|#

#|
____ __km tim fart kg __HM 1500 övr
2002 _?__ _?_ _?__ _? 2:02 _-__ _?_
2003 _?__ _?_ _?__ _? 2:11 _-__ _?_
2004 _?__ _?_ _?__ _? 2:07 _-__ _?_
2005 _?__ _?_ _?__ _? _-__ _-__ _?_
2006 _700 _70 6:00 79 2:01 _-__ _50
2007 _900 100 6:15 79 1:59 _-__ _60
2008 1400 140 6:00 80 1:51 5:50 _80
2009 1800 170 5:45 78 1:43 5:32 100
2010 1200 110 5:30 74 _-__ 5:25 _90
|#

;;; Mattias Larssson, MLarsson@dover-pidg.com

;;; 2002	HM	2:01:59
;;; 2003	HM	2:11:40
;;; 2004	8,8k	43:54
;;;		QM	56:13
;;;		HM	2:07:07
;;; 2005	QM	56:22
;;; 2006	6,7k	38:37
;;;		HM	2:01:24
;;; 2007	2,6k	11:47
;;;		HM	1:59:11
;;; 2008	800	2:44
;;;		1500	5:50
;;;		2,6k	11:23
;;;		3000	12:35
;;;		7,5k	35:34
;;;		10k	49:39
;;;		HM	1:51:21
;;; 2009	1000	3:26.7m
;;;		1500	5:32
;;;		5000	21:49
;;;		7,5k	33:26
;;;		HM	1:42:44
;;; 2010	100	13.06
;;;		200	26.8m
;;;		400	59.74
;;;		800	2:31.44
;;;		1000	3:18.5m
;;;		1500	5:25

;;; 400m: 60	6.7
;;; 800m: 76	5.2
;;; 1500m: 87	4.6
;;; 3000m: 97	4.0
;;; 5000m: 105	3.8
;;; 10K: -	3.6?
;;; HM: 117	3.4

;;; 10x200 @   35	6x600 @ 2:01
;;;  8x300 @   53	5x600 @ 1:58
;;;  4x400 @   71	4x600 @ 1:56
;;;  3x500 @ 1:29	3x600 @ 1:53
;;; ?2x600 @ 1:48	2x600 @ 1:51
;;;  1x600 @ 1:49	1x600 @ 1:49
;;;    800 @ 2:25	  800 @ 2:25

;;; http://www.newyorker.com/online/blogs/newsdesk/2010/11/video-alberto-salazar-dathan-ritzenhein.html

;; Frank Herbert: Whipping Star, Dosadi Experiment

(deftype utf8-lead-1 ()		'(integer 0 127))
(deftype utf8-lead-2 ()		'(integer 194 223))
(deftype utf8-lead-3 ()		'(integer 224 239))
(deftype utf8-lead-4 ()		'(integer 240 244))
(deftype utf8-lead-invalid ()	'(or (integer 128 193) (integer 245 255)))
(deftype utf8-trail ()		'(integer 128 191))
(deftype utf8-not-trail ()	'(or (integer 0 127) (integer 192 255)))
(deftype utf16-basic ()		'(or (integer 0 55295) (integer 57344 65535)))
(deftype utf16-hi-surrogate ()	'(integer 55296 56319))
(deftype utf16-lo-surrogate ()	'(integer 56320 57343))
(deftype utf16-surrogate ()	'(or utf16-hi-surrogate utf16-lo-surrogate))
(deftype utf16-supplementary ()	'(integer 65536 1114111))

(defun decode-uft8 (stream)
  (flet ((decode (n x min)
	   (loop repeat n for c = (funcall stream) do
	     (etypecase c
	       (null		(return-from decode (list :eof x)))
	       (utf8-trail	(setq x (+ (ash x 6) (logand c 63))))
	       (utf8-not-trail	(funcall stream c)
				(return-from decode (list :error x)))))
	   (cond ((< x min)		(list :overlong x))
		 ((<= 55296 x 57343)	(list :surrogate x))
		 ((> x 1114111)		(list :overbig x))
		 (t			x))))
    (let ((c (funcall stream)))
      (etypecase c
	(null			nil)
	(utf8-lead-1		c)
	(utf8-lead-2		(decode 1 (logand c 31) 128))
	(utf8-lead-3		(decode 2 (logand c 15) 2048))
	(utf8-lead-4		(decode 3 (logand c 7) 65536))
	(utf8-lead-invalid	(list :bad-lead (+ 56320 c)))))))

;       (let* ((c (funcall stream))
;	      (x (decode c)))
;	 (if error
;	     (list :bad (+ 56320 c))
;	     x))

(defun make-stream (list)
  (lambda (&optional x)
    (if x
	(push x list)
	(pop list))))

(defun decode-utf8-stream (stream)
  (loop for x = (decode-uft8 stream)
        while x
        do (format t "~X " x)))

(defun encode-utf8 (c)
  (flet ((encode (c code shift)
	   (loop for shift from shift upto 0 by 6
		 collect (+ code (logand (ash c shift) 63))
	         do (setq code 128))))
    (etypecase c
      ((integer 0 127)		(list c))
      ((integer 128 2047)	(encode c 192 -6))
      ((integer 2048 65535)	(encode c 224 -12))
      ((integer 65536 1114111)	(encode c 240 -18)))))

(defun decode-utf16 (stream)
  (flet ((decode-surrogates (c1 c2)
	   (typecase c2
	     (utf16-lo-surrogate	(+ 65536
					   (ash (logand c1 1023) 10)
					   (logand c2 1023)))
	     (t				(list :error c1 c2)))))
    (let ((c (funcall stream)))
      (etypecase c
	(null			nil)
	(utf16-basic		c)
	(utf16-hi-surrogate	(decode-surrogates c (funcall stream)))
	(utf16-lo-surrogate	(list :error c))))))

(defun encode-utf16 (c)
  (flet ((encode (c)
	   (let ((x (- c 65536)))
	     (list (+ 55296 (ash x -10))
		   (+ 56320 (logand x 1023))))))
    (etypecase c
      (utf16-basic		(list c))
      (utf16-supplementary	(encode c))
      (utf16-surrogate		:error))))

#|
SB 2006
HM: 2:01:24

SB 2007
2,6 km: 11:47
HM: 1:59:11

SB 2008
800m: 2:44
1500m: 5:50
3000m: 12:35
2,6 km: 11:23
7,5 km: 35:34
HM: 1:51:21

SB 2009
1000m: 3:27
1500m: 5:32
5000m: 21:49
7,5 km: 33:26
HM: 1:42:44

SB 2010
100m: 13.06
200m: 26.8m
400m: 61.5m
800m: 2:31.44
1000m: 3:18.8m
1500m: 5:25
|#

#|
	50000km : 2:53:21
	http://www.angelfire.com/electronic/ultramentor/records_running.html

	200m:	(29 +/- 4)%
	400m:	43 +/- 1
	800m:	66 +/- 2
	1500m:	84 +/- 1
|#

#|
http://www.canb.cat/
COMPLEX ESPORTIU MUNICIPAL PAU NEGRE - PARC MIGDIA
Complex Esportiu Municipal Mar Bella

Blvexch3
\\Blvdsksan32\Q
Q:\2-RESULTS - What\4-PRODUCT DEVELOPMENT\31-NPPDG\05-Timesheet\2010\

http://www.hemovningar.se/hem/oevningar/hoeft/snapping-hip.aspx

http://runningtimes.com/Article.aspx?ArticleID=19948
http://www.katysays.com/2010/06/02/you-dont-know-squat/
https://confluence1.imaje.intra:8443/dashboard.action
http://www.aud.edu/student_services/ath_facilities.asp
|#

;;; 5x600m @ 2:11[1:49] (80s)
;;; 5x800m @ 2:56[1:50] (120s)
;;; 4x1000m @ 3:45[1:53] (90s)

;;; My run is my drug: http://www.youtube.com/watch?v=vAhSZ501qEw
;;; Rodd: http://www.youtube.com/watch?v=eqVmMd7FdAA

;;; http://www.youtube.com/watch?v=Fk_sYLAczNQ
;;; http://www.youtube.com/watch?v=J3KaOdd87fA
;;; http://www.simbutiken.se/se/art/badskor-aquasocks-badtofflor.php
;;; http://svtplay.se/v/1654535/vad_ar_egentligen_en_svenskgren
;;; http://www.psim.se/personlig%20tr%C3%A4nare%20l%C3%B6pning.htm
;;; http://www.residentadvisor.net/feature.aspx?1144
;;; http://www.sct.nu/traning/ovningar/fotvad/fotvad.html

;;; http://www.youtube.com/watch?v=pgIn0ABZhT8
;;; http://www.youtube.com/watch?v=0dOzNG6keS8
;;; http://www.youtube.com/watch?v=WcrWmT27xLk
;;; http://www.fitsugar.com/Exercise-Video-Clam-Series-551450

;;; Qrn-9lDVrrs
#|
mencoder -of lavf -oac lavc -ovc lavc -lavfopts format=mp4 -lavcopts vcodec=mpeg4 /mnt/Videos/The\ Curious\ Case\ of\ Benjamin\ Button.avi -o Bejamin.mp4

mencoder -of lavf -lavfopts format=mp4 -oac lavc -ovc lavc -lavcopts aglobal=1:vglobal=1:acodec=libfaac:abitrate=128:vcodec=mpeg4 -af lavcresample=44100 video/RoyksoppRemember.flv -o video/R4.mp4

/usr/bin/mencoder -of lavf -lavfopts format=mp4 -oac lavc -ovc lavc -lavcopts aglobal=1:vglobal=1:acodec=libfaac:abitrate=128:vcodec=mpeg4:keyint=25 -ofps 25 -af lavcresample=44100 -vf harddup,scale=640:262 -mc 0 -noskip video/THE\ CURIOUS\ CASE\ of\ BENJAMIN\ BUTTON\ \(2008\)\ PROPER\ DVDRIP\ -\ SUPER\ EXCELLENT\ VIDEO\ and\ AUDIO.avi -o video/B5.mp4

mencoder -of lavf -lavfopts format=mp4 -oac lavc -ovc lavc -lavcopts aglobal=1:vglobal=1:acodec=libfaac:vcodec=mpeg4 -vf harddup,scale=640:262 video/THE\ CURIOUS\ CASE\ of\ BENJAMIN\ BUTTON\ \(2008\)\ PROPER\ DVDRIP\ -\ SUPER\ EXCELLENT\ VIDEO\ and\ AUDIO.avi -o video/B6.mp4

HandBrakeCLI -i RoyksoppRemember.mp4 -e x264 -x bitrate=384:me=hex:dct_decimate:nointerlaced:no8x8dct:nofast_pskip:trellis=0:partitions=p8x8,i4x4:mixed_refs:keyint=250:keyint_min=25:psy_rd=0.8,0.0:frameref=2:bframes=0:b_adapt=0:b_pyramid=none:noweight_b:weightp=0:direct_pred=none:subq=6:nombtree:chroma_me:nocabac:aq_mode=2:deblock:vbv_maxrate=384:vbv_bufsize=900:level_idc=12:ssim:psnr -f mp4 -o R10.mp4

HandBrakeCLI -i RoyksoppRemember.mp4 -e x264 -x no8x8dct:trellis=0:partitions=p8x8,i4x4:psy_rd=0.8,0.0:frameref=2:bframes=0:b_adapt=0:b_pyramid=none:noweight_b:weightp=0:direct_pred=none:subq=6:nombtree:nocabac -f mp4 -o R13.mp4

HandBrakeCLI -i Lotta2.mp4 -X 352 -Y 288 -e x264 -x no8x8dct:trellis=0:partitions=p8x8,i4x4:psy_rd=0.8,0.0:frameref=2:bframes=0:b_adapt=0:b_pyramid=none:noweight_b:weightp=0:direct_pred=none:subq=6:nombtree:nocabac -f mp4 -o L6.mp4

HandBrakeCLI -i Lotta2.mp4 -X 352 -Y 288 -e x264 -x no8x8dct:bframes=0:nocabac:weightp=0 -f mp4 -o L15.mp4
|#

;;; http://helionfilm.se/film/Loparen.mov

;;; General Strength Training for Runners
;;; http://runningtimes.com/Article.aspx?ArticleID=16625

#|
* Styrka
  - Medeltunga vikter, 8-12 reps
  - Tunga vikter, 1-5 reps
* Explosivitet
  - Medeltunga vikter, 1-5 reps
  - Backsprint
* Styrkeuthållighet
  - Lätta vikter, 15-30 reps
  - Längre backar

* Start
  - Liggande
  - Fallande
  - Trepunkt
  - Block
* Acceleration
  - 20-60m
* Maxfart
  - Flygande 20-50m, 95-99%
* Sprintuthållighet
  - 200-350m 95-100%
  - 300-600m 90-95%
|#

;;; I heart huckabees

;;; http://runningtimes.com/Article.aspx?ArticleID=17578
;;; http://www.youtube.com/watch?v=G-oAx-nD1gc
;;; http://video.google.com/videoplay?docid=338891915924595210

;;; Bolt:
;;; 2008, 100m: 9.69 (2.87 1.78 1.67 1.64 1.73)
;;; 2008, 200m: 19.30 = 9.98 + 9.32
;;; 2009, 100m: 9.58 (5.47 + 4.11) (2.89 1.75 1.67 1.61 1.66)
;;; 2009, 200m: 19.19 = 9.92 + 9.27 (5.60 4.32 4.52 4.75)
;;; 2010, 400m: 45.86
;;; 2011, 200m: 19.40 = 9.97 + 9.43

;;; Blake:
;;; 2011, 200m: 19.26 = 10.14 + 9.12 (?)

;;; Johnson:
;;; 1994, 100m: 10.09
;;; 1996, 200m: 19.32 = 10.12 + 9.20
;;; 1999, 400m: 43.18 = 11.10 + 10.12 + 10.44 + 11.52
;;;                     6.14 4.96 5.00 5.12 5.20 5.24 5.52 6.00

;;; Kipketer:
;;; 1996, 800m: 1:42.5  = 50.1 + 52.5 (12.3 12.0 12.9 12.9 13.1 13.1 12.9 13.4)
;;; 1997, 800m: 1:41.11 = 49.3 + 51.8 (23.8 25.5 25.3 26.5)

;;; Rudisha:
;;; 2010, 800m: 1:41.51 = 49.65 + 51.86 (49.65 24.75 27.11)
;;; 2010, 800m: 1:41.09 = 49.1 + 52.0 (1:14.54)
;;; 2010, 800m: 1:41.01 = 48.9 + 52.1 (1:14.59)
;;; 2012, 800m: 1:40.91 = 49.28 + 51.63 (23.5 25.8 25.0 26.6)

;;; Coe:
;;; 1981, 800m: 1:41.73 = 49.7 + 52.0 (24.6 25.3 25.1 26.7)

;;; Ngeny:
;;; 1000m: 2:11.96 = 1:44.62 27.34  ?49.66 53.9 27.4

;;; Kimeli Teimet, 2008:
;;; 61:21 ((:work 14:19 5.0) (:work 14:11 5.0) (:work 14:35 5.0) (:work 15:14 5.0) (:work 3:02 1.1))
;;; Sammy Kirui, 2010: 61:10
;;; 14:38 28:29 42:39 57:54 61:07
;;; 14:38 13:51 14:10 15:15  3:16
;;; Albert Kiplagat Matebor: 60:52
;;; 14:27 28:22 42:52 57:38
;;; Melkevik Otterbu, 2008:
;;; ((:work 16:30 5.0) (:work 16:43 5.0) (:work 16:41 5.0) (:work 16:50 5.0) (:work 3:35 1.1))
;;; Stine Larsen, 2000: 69:28
;;; Joyce Chepkirui, 2011: 69:04
;;; 15:59 31:41 48:13 1:05:25

;;; Gebrselassie
;;; 2008, marathon: 14:35 29:12 44:03 58:50 (HM 62:03)
;;; 1:13:40 1:28:25 1:43:05 1:47:34 2:03:59

;;; Makau
;;; 2011, marathon: 14:36 29:17 43:51 58:30 (HM 61:43)
;;; 1:13:18 1:27:38 1:42:16 1:57:15 2:03:38

;;; Kipsang
;;; 2011, marathon: 14:49 29:26 43:59 58:31 (HM 61:40)
;;; 1:13:09 1:27:49 1:42:43 1:57:19 2:03:42

(defun mandel ()
  (loop for y from -1 to 1.1 by 0.1 do
     (loop for x from -2 to 1 by 0.04 do
	(loop for z = 0 then (+ (* z z) (complex x y))
	      for c from 127 downto 33
	      while (< (abs z) 2)
	      finally (write-char (code-char c))))
     (terpri)))

(defun mandel ()
  (do ((y -1 (+ y .1)))
      ((>= y 1))
    (do ((x -2 (+ x .04)))
	((>= x 1))
      (do ((z 0 (+ (* z z) (complex x y)))
	   (c 127 (- c 1)))
	  ((or (> (abs z) 2) (= c 32))
	   (write-char (code-char c)))))
    (terpri)))

;; http://www.justin.tv/siaki
;; http://www.nzonscreen.com/title/peter-snell---athlete-1964
;; http://www.youtube.com/watch?v=9wI-9RJi0Qo

#|
110mH	Hurdle 1	13.72	96.28
Blue	Hurdle 2	22.86	87.14
	Hurdle 3	32.00	78.00
13.72+	Hurdle 4	41.14	68.86
n*9.14	Hurdle 5	50.28	59.72
	Hurdle 6	59.42	50.58
	Hurdle 7	68.56	41.44
	Hurdle 8	77.70	32.30
	Hurdle 9	86.84	23.16
	Hurdle 10	95.98	14.02


100mH	Hurdle 1	13.00	87.00
Yellow	Hurdle 2	21.50	78.50
	Hurdle 3	30.00	70.00
13.00+	Hurdle 4	38.50	61.50
n*8.50	Hurdle 5	47.00	53.00
	Hurdle 6	55.50	44.50
	Hurdle 7	64.00	36.00
	Hurdle 8	72.50	27.50
	Hurdle 9	81.00	19.00
	Hurdle 10	89.50	10.50

400mH	Hurdle 1	 45	355	  0
Green	Hurdle 2	 80	320	 35
	Hurdle 3	115	295	 70
45+	Hurdle 4	150	250	105
n*35	Hurdle 5	185	215	140
	Hurdle 6	220	180	175
	Hurdle 7	255	145	210
	Hurdle 8	290	110	245
	Hurdle 9	325	 75	280
	Hurdle 10	360	 40	315

20	60 -> 400#10
25	110#3 -> 100#5
30	100#3 -> 400#10, 100 -> 100#3
35	400#9 -> 400#10
40	400#10 -> Finish, 100 -> 60, 110 -> 100#3
45	400 -> 400#1, 190 -> 400#7
50	300 -> 400#4, 400#4 -> 200
55	200 -> 400#7
60	60 -> Finish
65	210 -> 400#7
70	100#3 -> Finish, 110 -> 400#10, 400#7 -> 400#9
75	400#9 -> Finish, 400#7 -> 100#3, 290 -> 400#5
80	80 -> Finish
85	300 -> 400#5
90	190 -> Finish
95	310 -> 400#5
100	100 -> Finish
110	110 -> Finish
120	200 -> 80, 190 -> 100#3
130	200 -> 100#3
140	210 -> 100#3, 200 -> 60
150	400 -> 400#4, 190 -> 400#10
160	200 -> 400#10
170	210 -> 400#10
180	400#4 -> 100#3, 290 -> 110
190	190 -> Finish
200	200 -> Finish
210	210 -> Finish
220	300 -> 80
230	300 -> 100#3
240	300 -> 60
250	400#4 -> Finish, 290 -> 400#10, 400#2 -> 100#3
260	300 -> 400#10
270	310 -> 100#10
280	400#2 -> 400#10
290	290 -> Finish
300	300 -> Finish
320	400#2 -> Finish
340	400 -> 60
350	400#4 -> Finish
360	400 -> 400#10
381	400 -> 100#9
|#

(defun hurdle (n m)
  (ecase n
    (0		m)
    (11		0)
    ((1 2 3 4 5 6 7 8 9 10)
     (decf n)
     (- m
	(ecase m
	  (100	(+ 13 (* n 8.5)))
	  (110	(+ 13.72 (* n 9.14)))
	  (400	(+ 45 (* n 35))))))))

(defvar *work-types* '(:work :hill))

(defun workp (x)
  (when (consp x)
    (setq x (first x)))
  (member x *work-types*))

(defvar *split-distance* 0) 
(defvar *split-time* 0)

(defun workout-splits (workout &key (split 1.0) (work-only t) (start nil))
  (when work-only
    (setq workout (remove-if-not #'workp workout)))
  (when (numberp split)
    (setf split (list split))
    (setf (rest split) split))
  (labels ((output (work)
	     (format t "~&~5,2F ~9<~A~> ~,2F ~7<~A~> ~A/km"
		     (+ *split-distance* (workout-distance work))
		     (minutes (+ *split-time* (workout-time work)))
		     (workout-distance work)
		     (minutes (workout-time work))
		     (minutes (/ (workout-time work) (workout-distance work))))
	     (when (workout-hr work)
	       (format t " ~D/~D"
		       (round (workout-hr work))
		       (workout-hr-max work)))
	     (terpri))
	   (first-split (workout split-workout)
	     (if (or (null workout)
		     (>= (workout-distance split-workout)
			 (or start (first split))))
		 (values split-workout workout)
		 (first-split (rest workout)
			      (append split-workout (list (first workout)))))))
    (cond
      ((null workout))
      ((zerop (workout-distance workout)))
      ((< (workout-distance workout) (or start (first split) 1e30))
       (output workout))
      (t
       (multiple-value-bind (split-work workout) (first-split workout nil)
	 (let* ((work (car (last split-work)))
		(x (- (workout-distance split-work) (or start (first split))))
		(y (/ x (work-dist work)))
		(z (* y (second work)))
		(hr (list (hr work) :max (hr-max work)))
		(this-split
		 (append (butlast split-work)
			 (list (list :work
				     (- (second work) z)
				     (- (work-dist work) x)
				     :hr hr)))))
	   (output this-split)
	   (let ((*split-distance* (+ *split-distance*
				      (workout-distance this-split)))
		 (*split-time* (+ *split-time*
				  (workout-time this-split))))
	     (workout-splits (if (zerop z)
				 workout
				 (cons (list :work z x :hr hr) workout))
			     :split (rest split)
			     :work-only work-only))))))))

(defun print-workout-kolozzeum (workout)
  (dolist (x (workout-bundle workout))
    (let ((distance (third x))
	  (time (second x))
	  (hr (hr x)))
      (when (numberp hr)
	(setf hr (round hr)))
      (if (eq (first x) :rest)
	  (format t "~&~A vila, ~D\\~D~%"
		  (minutes time t)
		  hr
		  (hr-min x))
	  (multiple-value-bind (distance-unit distance-km)
	      (if (integerp distance)
		  (values "m" (/ distance 1000.0))
		  (values "km" distance))
	    (format t "~&~D~A@~A/km, ~D/~D, ~A~%"
		    distance
		    distance-unit
		    (unless (zerop distance)
		      (minutes (/ time distance-km) t))
		    hr
		    (hr-max x)
		    (minutes time)))))))

(defun print-workout-jogg (workout)
  (dolist (x workout)
    (let ((distance (third x))
	  (time (second x))
	  (hr-max (hr-max x)))
      (format t "~&~A" (minutes time))
      (when (hr x)
	(format t ", ~D" (round (hr x))))
      (when hr-max
	(format t "/~D" hr-max))
      (when (and (consp (fifth x)) (hr-min x))
	(format t "\\~D" (hr-min x)))
      (format t (cond
		  ((eql distance 1.0)	"~%")
		  ((integerp distance)	", ~Dm~%")
		  ((rationalp distance)	", ~,1Fm~%")
		  (t			", ~,2Fkm~%"))
	      distance))))

(defun workout-bundle (workout)
  (labels ((max-hr (hr1 hr2)
	     (cond ((null hr1)	hr2)
		   ((null hr2)	hr1)
		   (t		(max hr1 hr2))))
	   (bundle (work1 work2)
	     (list :work
		   (+ (second work1) (second work2))
		   (if (or (floatp (third work1))
			   (floatp (third work2)))
		       (+ (work-dist work1) (work-dist work2))
		       (+ (third work1) (third work2)))
		   :hr
		   (list (average-hr (list (second work1)
					   (second work2))
				     (list (hr work1)
					   (hr work2)))
			 :max
			 (max-hr (hr-max work1)
				 (hr-max work2))))))
    (if (null workout)
	nil
	(let ((work1 (first workout))
	      (workout (workout-bundle (rest workout))))
	  (if (null workout)
	      (list work1)
	      (let ((work2 (first workout)))
		(if (and (eq (first work1) :work)
			 (eq (first work2) :work))
		    (cons (bundle work1 work2)
			  (rest workout))
		    (list* work1 work2 (rest workout)))))))))

(defun workout-work (workout)
  (remove-if-not #'workp workout))

(defun workout-type (type workout)
  (remove type workout :key #'first :test-not #'eq))

(defun workout-time (workout)
  (reduce #'+ workout :key #'second))

(defun hr (work)
  (let ((hr (fifth work)))
    (if (numberp hr)
	hr
	(first hr))))

(defun workout-hr (workout)
  (let ((times (mapcar #'second workout))
	(hrs (mapcar #'hr workout)))
    (unless (or (null times) (null hrs) (member nil hrs))
      (average-hr times hrs))))

(defun hr-max (work)
  (let ((hr (fifth work)))
    (cond
      ((numberp hr)	hr)
      ((listp hr)	(getf (rest hr) :max)))))

(defun hr-min (work)
  (let ((hr (fifth work)))
    (cond
      ((numberp hr)	hr)
      ((listp hr)	(or (getf (rest hr) :min)
			    (getf (rest hr) :end))))))

(defun workout-hr-max (workout)
  (reduce (lambda (x y)
	    (if (numberp x)
		(if (numberp y)
		    (max x y)
		    x)
		y))
	  workout
	  :key #'hr-max))

(defun work-dist (work)
  (let ((d (third work)))
    (cond
      ((null d)		0)
      ((integerp d)	(/ d 1000.0))
      ((typep d 'ratio)	(/ d 1000))
      (t		d))))

(defun workout-distance (workout)
  (reduce #'+ workout :key #'work-dist))

(defun print-workout (workout)
  (print-summary :warm workout)
  (cond
    ((member :hill workout :key #'first)
			(print-hill-summary workout))
    ((member :rest workout :key #'first)
			(print-interval-summary (workout-bundle workout)))
    (t
			(print-distance-summary workout)))
  #|
  (if (member :rest workout :key #'first)
      (print-interval-summary (workout-bundle workout))
      (print-distance-summary workout))
  (when (member :hill workout :key #'first)
    (print-hill-summary workout))
  |#
  (print-summary :cool workout)
  (print-totals (remove :rest workout :key #'first))
  (print-totals workout))

(defun print-interval-summary (workout)
  (let* ((work (workout-work workout))
	 (reps (length work))
	 (time (workout-time work))
	 (distance (workout-distance work))
	 (hr (workout-hr work))
	 (first-work-position (position-if #'workp workout))
	 (last-work-position (position-if #'workp workout :from-end t))
	 (rest-between-work (subseq workout first-work-position last-work-position))
	 (rest (remove :rest rest-between-work :key #'first :test-not #'eq)))
    (format t "~&Work: ~Dx~D @ ~A (~A/km)"
	    reps
	    (round (/ distance reps .001))
	    (minutes (/ time reps))
	    (minutes (/ time distance)))
    (when hr
      (format t " HR ~D" (round hr)))
    (when rest
      (let ((avg-rest (/ (reduce #'+ rest :key #'second) (length rest)))
	    (middle-rest (second (nth (floor (1- (length rest)) 2) rest))))
	(if (and (oddp (length rest))
		 (> (/ middle-rest avg-rest) 1.9))
	    (format t " [~A / ~A]~%"
		    (minutes (/ (- (* avg-rest (length rest)) middle-rest)
				(1- (length rest))))
		    (minutes middle-rest))
	    (format t " [~A]~%" (minutes avg-rest)))))))

(defun hillp (x)
  (when (consp x)
    (setq x (first x)))
  (eq x :hill))

(defun print-hill-summary (workout)
  (let* ((work (workout-type :hill workout))
	 (reps (length work))
	 (time (workout-time work))
	 (distance (workout-distance work))
	 (hr (workout-hr work))
	 (first-hill (position-if #'hillp workout))
	 (last-hill (position-if #'hillp workout :from-end t))
	 (rest-between-work (subseq workout first-hill last-hill))
	 (rest (remove :rest rest-between-work :key #'first :test-not #'eq)))
    (format t "~&Hills: ~Dx~D @ ~A (~A/km)"
	    reps
	    (round (/ distance reps .001))
	    (minutes (/ time reps))
	    (minutes (/ time distance)))
    (when hr
      (format t "/~D" (round hr)))
    (when rest
      (let ((avg-rest (/ (reduce #'+ rest :key #'second) (length rest)))
	    (middle-rest (second (nth (floor (1- (length rest)) 2) rest))))
	(if (and (oddp (length rest))
		 (> (/ middle-rest avg-rest) 1.9))
	    (format t " [~A / ~A]~%"
		    (minutes (/ (- (* avg-rest (length rest)) middle-rest)
				(1- (length rest))))
		    (minutes middle-rest))
	    (format t " [~A]~%" (minutes avg-rest)))))))

(defun print-summary (type workout)
  (let* ((work (workout-type type workout))
	 (time (workout-time work))
	 (distance (workout-distance work))
	 (hr (workout-hr work)))
    (when work
      (format t "~&~:(~A~): ~,2fkm@~A"
	      type
	      distance
	      (minutes (/ time distance)))
      (when hr
	(format t "/~D" (round hr)))
      (terpri))))

(defun print-distance-summary (workout)
  (let* ((work (workout-work workout))
	 (time (workout-time work))
	 (distance (workout-distance work))
	 (hr (workout-hr work)))
    (unless (zerop distance)
      (format t "~&Work: ~,2Fkm@~A"
	      distance
	      (minutes (/ time distance)))
      (when hr
	(format t "/~D" (round hr))))
    (terpri)))

(defun print-totals (workout)
  (let ((distance (workout-distance workout))
	(time (workout-time workout))
	(hr (workout-hr workout)))
    (format t "~&Totals: ~,2Fkm, ~A, ~A/km"
	    distance
	    (minutes time)
	    (minutes (/ time distance)))
    (when hr
      (format t ", ~A/~A (~D%)"
	      (round hr)
	      (workout-hr-max workout)
	      (round (/ hr 1.95))))
    (terpri)))

(defun split-sequence (delimiter sequence)
  (let ((result nil)
	(current nil))
    (sb-sequence:dosequence (x sequence)
      (cond
	((eql x delimiter)
	 (push (nreverse current) result)
	 (setq current nil))
	(t
	 (push x current))))
    (push (nreverse current) result)
    (setq result (nreverse result))
    (mapcar (typecase sequence
	      (string	(lambda (x) (coerce x 'string)))
	      (vector	(lambda (x) (coerce x 'vector)))
	      (list	(lambda (x) (coerce x 'list))))
	    result)))

(defmacro when-match (vars separator val &body body)
  (let ((list (gensym)))
    `(let ((,list (split-sequence ,separator ,val)))
       (when (eql (length ,list) ,(length vars))
	 (let ,(let ((n -1))
		 (mapcar (lambda (var) `(,var (nth ,(incf n) ,list)))
			 vars))
	   ,@body)))))

(defun print-week (year month day)
  (flet ((this-week-p (sym year month day)
	   (when-match (x stuff y) #\* (symbol-name sym)
	     (declare (ignore x y))
	     (when-match (x date time) #\/ stuff
	       (declare (ignore x time))
	       (when-match (y m d) #\- date
		 (and (eql year (parse-integer y))
		      (eql month (parse-integer m))
		      (eql day (parse-integer d))))))))
    (let ((date (encode-universal-time 0 0 12 day month year))
	  (workouts nil))
      (dotimes (i 7)
 	(multiple-value-bind (x1 x2 x3 day month year)
	    (decode-universal-time date)
	  (declare (ignore x1 x2 x3))
	  (do-symbols (sym)
	    (when (and (boundp sym)
		       (this-week-p sym year month day))
	      (setq workouts (append (symbol-value sym) workouts))))
	  (incf date (* 3600 24))))
      (unless (null workouts)
	(print-totals workouts)
	(print-distance-summary (remove :rest workouts :key #'first))))))

(defun print-weeks (year month day weeks)
  (let ((date (encode-universal-time 0 0 12 day month year)))
    (dotimes (i weeks)
      (multiple-value-bind (x1 x2 x3 day month year)
	  (decode-universal-time date)
	(declare (ignore x1 x2 x3))
	(format t "~%Week starting ~D-~2,'0D-~2,'0D" year month day)
	(print-week year month day)
	(incf date (* 3600 24 7))))))

(defun print-month (year month)
  (flet ((this-month-p (sym year month)
	   (when-match (x stuff y) #\* (symbol-name sym)
	     (declare (ignore x y))
	     (when-match (x date time) #\/ stuff
	       (declare (ignore x time))
	       (when-match (y m d) #\- date
		 (declare (ignore d))
		 (and (eql year (parse-integer y))
		      (eql month (parse-integer m))))))))
    (let ((workouts nil))
      (do-symbols (sym)
	(when (and (boundp sym)
		   (this-month-p sym year month))
	  (setq workouts (append (symbol-value sym) workouts))))
      (unless (null workouts)
	(print-totals workouts)
	(print-distance-summary (remove :rest workouts :key #'first))))))

(defun print-months (year month months)
    (dotimes (i months)
      (format t "~%Month ~D-~2,'0D-" year month)
	(print-month year month))
      (incf month 1)
      (when (= month 13)
	(setq month 1)
	(incf year)))

(defun beats-per-km (workout &optional resting-heartrate)
  (let* ((work (workout-work workout))
	 (time (workout-time work))
	 (distance (workout-distance work))
	 (hr (workout-hr work)))
    (unless (or (null hr) (zerop distance))
      (when resting-heartrate
	(decf hr resting-heartrate))
      (* hr (/ time distance)))))

(defun map-workouts (fn &key start end)
  (loop
     for date from (designator-time (or start '(2008 04 01)))
              below (designator-time (or end '(2010 01 01)))
	      by (* 3600 24)
     append 
       (flet ((this-day-p (sym year month day)
		(when-match (x stuff y) #\* (symbol-name sym)
		  (declare (ignore x y))
		  (when-match (x date time) #\/ stuff
		    (declare (ignore x time))
		    (when-match (y m d) #\- date
		      (and (eql year (parse-integer y))
			   (eql month (parse-integer m))
			   (eql day (parse-integer d))))))))
	 (multiple-value-bind (x1 x2 x3 day month year)
	     (decode-universal-time date)
	  (declare (ignore x1 x2 x3))
	  (let ((result nil))
	    (do-symbols (sym)
	      (when (and (boundp sym)
			 (this-day-p sym year month day))
		(push (funcall fn (symbol-value sym) year month day) result)))
	    (nreverse result))))))

(defun designator-time (time)
  (etypecase time
    (integer time)
    ((cons integer (cons integer (cons integer null)))
     (encode-universal-time 0 0 12 (third time) (second time) (first time)))))

(defmacro do-workouts ((workout (&optional year month day) &key start end) &body body)
  (let ((year (or year (gensym)))
	(month (or month (gensym)))
	(day (or day (gensym))))
    `(map-workouts (lambda (,workout ,year ,month ,day)
		     (declare (ignorable ,year ,month ,day))
		     ,@body)
		   :start ,start
		   :end ,end)))

(defun print-workout-beats-per-km (&optional start end)
  (do-workouts (workout (year month day) :start start :end end)
    (unless (member :rest workout :key #'first)
      (let ((work (workout-work workout)))
	(unless (zerop (workout-distance work))
	  (format t "~& ~D-~2,'0D-~2,'0D: ~4,1Fkm @ ~A/km: ~,1F beats/km (~,1F)~%"
		  year
		  month
		  day
		  (workout-distance work)
		  (minutes (/ (workout-time work) (workout-distance work)))
		  (beats-per-km work)
		  (beats-per-km work 55))))))
  (values))

(defun plot-filter (workout &key
		    (remove-first-minutes 12)
		    (keep-first-minutes 60)
		    (remove-hr-above 175))
  (labels ((remove-initial-minutes (workout minutes)
	     (if (or (null workout) (<= minutes 0))
		 workout
		 (remove-initial-minutes
		  (rest workout) (- minutes (second (first workout))))))
	   (keep-initial-minutes (workout minutes result)
	     (let ((x (second (first workout))))
	       (if (or (null workout) (> x minutes))
		   (nreverse result)
		   (keep-initial-minutes (rest workout)
					 (- minutes x)
					 (cons (first workout) result))))))
    (unless (or (member :rest workout :key #'first)
		(member :hill workout :key #'first))
      (let* ((w1 (keep-initial-minutes workout keep-first-minutes nil))
	     (w2 (if (some (lambda (x) (rationalp (third x))) w1)
		     (remove :warm w1 :key #'first)
		     w1))
	     (w3 (remove-initial-minutes w2 remove-first-minutes))
	     (w4 (remove-if (lambda (x) (or (null x) (> x remove-hr-above)))
			    w3
			    :key #'hr))
	     (w5 (workout-work w4)))
	w5))))

(defun plot-workout-km-hr-and-speed (&key start end (file "TMP"))
  (with-open-file (*standard-output* file
				     :direction :output
				     :if-exists :supersede)
    (do-workouts (workout (year month day) :start start :end end)
      (let ((work (plot-filter workout)))
	(unless (zerop (workout-distance work))
	  (print work *terminal-io*)
	  (dolist (x work)
	    (format t "~& ~,2F ~D~%"
		    (/ (* 1000 (work-dist x)) (second x))
		    (hr x)))))))
  (values))

(defun make-population (&optional (params (list 1.6 (/ 10.5 60) 200)))
  (flet ((mutate (x)
	   (let ((r1 (random 1.0))
		 (r2 (random 1.0)))
	     (* x (+ 1 (* .2 (+ r1 r2 -1)))))))
    (loop repeat 1000 collect
	 (loop for x in params collect (mutate x)))))

(defun next-generation (population &optional f)
  (flet ((f (x individual)
	   (+ (first individual)
	      (* (second individual)
		 (log (/ x (third individual)) 2)))))
    (unless f
      (setq f #'f))
    (let ((n (length population)))
      (let ((children
	     (loop repeat 200
		collect
		  (let ((p1 (random n))
			(p2 (random n)))
		    (offspring (nth p1 population) (nth p2 population))))))
	(flet ((fittest (x y)
		 (< (fitness x f) (fitness y f))))
	  (subseq (sort (nconc children population) #'fittest) 0 n))))))

(defun fitness (individual f)
  (labels ((square (x)
	     (* x x))
	   (g (x)
	     (square (- (funcall f (first x) individual)
			(second x)))))
    (reduce #'+ (mapcar #'g *x*))))

(defun min/km (meters)
  (let ((y1
	 ;; Long distance:
	 (let* ((x '(1.760176 0.15384239 205.77406))
		(a (first x))
		(b (second x))
		(c (third x)))
	   (+ a (* b (log (/ meters c) 2)))))
	(y2
	 ;; Middle distance:
	 (let* ((x '(1.7735754 0.17880425 213.76967))
		(a (first x))
		(b (second x))
		(c (third x)))
	   (+ a (* b (log (/ meters c) 2)))))
	(y3 (cond ((<= meters 3000) 0)
		  ((>= meters 10000) 1)
		  (t (expt (/ (- meters 3000) 7000) .5)))))
    (+ (* y3 y1) (* (- 1 y3) y2))))

(defun offspring (parent1 parent2)
  (flet ((random-parent (x y)
	   (if (< (random 1.0) .5) x y))
	 (mutate (x)
	   (if (< (random 1.0) .8)
	       x
	       (let ((r1 (random 1.0))
		     (r2 (random 1.0)))
		 (* x (+ 1 (* .5 (+ r1 r2 -1))))))))
    (loop for x in parent1 and y in parent2
	 collect (mutate (random-parent x y)))))

#|
OS 1924: -
OS 1928: 100, 800
OS 1932: 100
OS 1936: 100
OS 1948: 100, 200
OS 1952: 100, 200
OS 1956: 100, 200
OS 1960: 100, 200, 800
OS 1964: 100-800
OS 1968: 100-800
OS 1972: 100-1500
OS 1976: 100-1500
OS 1980: 100-1500
VM 1983: 100-3000, marathon
OS 1984: 100-3000, marathon
VM 1987: 100-3000, 10000-marathon
OS 1988: 100-3000, 10000-marathon
VM 1991: 100-3000, 10000-marathon
OS 1992: 100-3000, 10000-marathon
VM 1993: 100-3000, 10000-marathon
VM 1995: 100-1500, 5000-marathon
OS 1996: 100-1500, 5000-marathon
VM 1997: 100-1500, 5000-marathon
VM 1999: 100-1500, 5000-marathon
OS 2000: 100-1500, 5000-marathon
VM 2001: 100-1500, 5000-marathon
VM 2003: 100-1500, 5000-marathon
OS 2004: 100-1500, 5000-marathon
VM 2005: 100-marathon
VM 2007: 100-marathon
OS 2008: 100-marathon
|#

(defun lottas-bok (&key (pages 342)
		   (start-day 2) (start-month 9)
		   (end-day 15) (end-month 11))
  (let* ((day1 (encode-universal-time 0 0 0 start-day start-month 2008))
	 (day2 (encode-universal-time 0 0 0 end-day end-month 2008))
	 (days (+ (round (- day2 day1) (* 24 3600)) 2)))
    (dotimes (i days)
      (multiple-value-bind (sec min hr day month)
	  (decode-universal-time (+ day1 (* i 3600 24)))
	(declare (ignore sec min hr))
	(format t "~& ~D/~D: sida ~D ~%"
		day
		month
		(ceiling (* pages (/ (1+ i) days))))))))

(defun run-energy (meters minutes)
  (* meters (expt (/ meters minutes 60) 3)))

(defun energy-minutes (energy meters)
  (expt (/ (expt meters 4) energy) 1/3))

(defun linear-regression-factor (x y)
  (let ((n (length x)))
    (/ (- (* n (reduce #'+ (mapcar #'* x y)))
	  (* (reduce #'+ x) (reduce #'+ y)))
       (- (* n (reduce #'+ (mapcar #'* x x)))
	  (expt (reduce #'+ x) 2)))))

(defun linear-regression-term (x y)
  (/ (- (reduce #'+ y)
	(* (linear-regression-factor x y)
	   (reduce #'+ x)))
     (length x)))

(let* ((records '(( 3000	 7:20.67)
		  ( 5000	12:37.35)
		  (10000	26:17.53)
		  (21097.5	58:23)))
       (speed (loop for (x y) in records collect (/ x y 60)))
       (x (loop for (d) in records collect (log d)))
       (factor (linear-regression-factor x speed))
       (term (linear-regression-term x speed)))
  (defun aerobic-speed (distance)
    (+ (* factor (log distance)) term)))

(let* ((records '((200		0:19.19)
		  (400		0:43.18)
		  (800		1:41.11)))
       (speed (loop for (x y) in records collect (/ x (- y 0:1.2) 60)))
       (x (loop for (d) in records collect (log d)))
       (factor (linear-regression-factor x speed))
       (term (linear-regression-term x speed)))
  (defun anaerobic-speed (distance)
    (let* ((speed (+ (* factor (log distance)) term))
	   (time (/ distance speed)))
      (/ distance (+ time 1.2)))))

(let ((records '((  100		   0:09.58)
		 (  200		   0:19.19)
		 (  400		   0:43.18)
		;(  600		   1:12.81)
		 (  800		   1:41.01)
		;( 1000		   2:11.96)
		 ( 1500		   3:26.00)
		 ( 3000		   7:20.67)
		 ( 5000		  12:37.35)
		 (10000		  26:17.53)
		 (21097.5	  58:23)
		 (42195		2:03:38)
		 (60000		3:01:50))))
  (flet ((interpolate-time (d1 t1 d2 t2 d3)
	   (let ((x (log d3))
		 (x1 (log d1))
		 (y1 (/ d1 t1))
		 (x2 (log d2))
		 (y2 (/ d2 t2)))
	     ;;t=x/(a + (b-a)(log x - c)/(d-c)
	     (/ d3 (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1)))))))
	 (interpolate-distance (d1 t1 d2 t2 t3)
	   (let* ((v1 (float (/ d1 t1) 1d0)) ;a
		  (v2 (/ d2 t2)) ;b
		  (x1 (log d1))  ;c
		  (x2 (log d2))  ;d
		  (v1-v2 (- v1 v2))
		  (x2-x1 (- x2 x1))
		  (v1x2-v2x1/v1-v2 (/ (- (* v1 x2) (* v2 x1)) v1-v2))
		  (t3v1-v2/x2-x2 (/ (* t3 v1-v2) x2-x1)))
	     ;;x=t(a-b)W( (d-c)pow(exp(d-bc/a),a/(a-b))/t(a-b) )/(d-c)
	     ;;x=t(a-b)W( (d-c)exp(ad-bc/(a-b))/t(a-b) )/(d-c)
	     (* t3v1-v2/x2-x2
		(lambert-w (/ (exp v1x2-v2x1/v1-v2) t3v1-v2/x2-x2))))))
    (defun wr-time (distance)
      (loop for ((d1 t1) (d2 t2)) on records
	    when (<= d1 distance d2)
	    return (interpolate-time d1 t1 d2 t2 distance)))
    (defun wr-distance (time)
      (loop for ((d1 t1) (d2 t2)) on records
	    when (<= t1 time t2)
	    return (interpolate-distance d1 t1 d2 t2 time)))))
            #|
	    return (labels ((foo (d3 d4)
			      (let ((d5 (* .5 (+ d3 d4))))
				(if (> (/ d3 d4) .999)
				    d5
				    (let ((t3 (interpolate d1 t1 d2 t2 d5)))
				      (if (< t3 time)
					  (foo d5 d4)
					  (foo d3 d5)))))))
		     (foo d1 d2)))))) |#

(defun lambert-w (x0)
  (check-type x0 (real #.(- (/ (exp 1))) *))
  (flet ((next-halley (x)
	   (let* ((ex (exp x))
		  (xex (* x ex))
		  (xex-x0 (- xex x0))
		  (x+1 (+ x 1d0)))
	     (- x (/ xex-x0
		     (- (* ex x+1)
			(/ (* (+ x 2d0) xex-x0)
			   (+ x+1 x+1)))))))
	 (next-newton (x)
	   (let* ((ex (exp x))
		  (xex (* x ex)))
	     (- x (/ (- xex x0) (+ xex ex)))))
	 (convergedp (x y)
	   (multiple-value-bind (x1 x2 x3) (decode-float x)
	     (multiple-value-bind (y1 y2 y3) (decode-float y)
	       (and (< (abs (- x1 y1)) 1d-15)
		    (eql x2 y2)
		    (eql x3 y3))))))
    (loop for x1 = (if (> x0 1d0)
		       (log (float x0 1d0))
		       1d0) then (next-halley x1)
	  and x2 = 0d0 then x1
	  until (convergedp x1 x2)
	  finally (return x1))))

(defun running-timer (&key (video-start 0)
		           (running-start 0)
		           (running-end 59.0)
		           (running-step 0.01))
  (loop for video-time from video-start by running-step
        for running-time from running-start by running-step below running-end
        do
        (format t "~D:~2,'0D:~6,3,,,'0F,"
		(truncate video-time 3600)
		(truncate video-time 60)
		video-time)
        (format t "~D:~2,'0D:~6,3,,,'0F~%"
		(truncate video-time 3600)
		(truncate video-time 60)
		video-time)
        (if (< running-end 60)
	    (format t "~,2F~%~%" running-time)
	    (format t "~D:~5,2,,,'0F~%~%"
		    (truncate running-time 60)
		    running-time))))

#|
56	1.55 1.50 1.55 1.55 ? 1.55 1.57 1.61 1.55 1.62 1.55 1.59 1.64
	1.61 1.54 ? 1.56 1.54 1.62 => 1.57

62	1.61 1.63 1.61 1.61 1.65 1.57 1.61 ? 1.58 ? 1.55 1.60 1.55
	1.57 1.63 1.60 1.61 => 1.60

69	1.68 1.67 1.65 1.62 ? 1.58 ? 1.64 1.62 1.68 ? 1.66 1.65 ? 1.63
	1.68 ? 1.60 1.53 ? 1.60 1.63 1.63 1.70 => 1.64

77	? 1.68 1.65 ? ? 1.63 1.72 1.67 1.70 ? 1.68 ? 1.65 1.75 1.60 1.68
	1.64 1.75 1.80 1.71 1.67 1.72 1.78 => 1.69

85	1.72 ? ? 1.73 ? 1.72 ? 1.68 ? 1.72 1.72 ? 1.65 1.72 1.75 1.72
	1.72 1.72 1.72 => 1.71

94	1.75 1.81 1.78 1.76 ? 1.78 1.70 1.77 1.75 1.72 1.76 1.79 1.76
	1.72 1.68 => 1.75

105	? 1.82 ? 1.79 1.73 1.75 1.85 1.80 1.75 1.80 1.77 1.81 1.81 1.82
	1.83 1.82 1.70 1.76 1.81 1.81 => 1.79

 54 156
 56 157
 59 161
 62 160
 64 163
 69 164
 69 166
 70 164
 76 170
 77 169
 83 171
 85 171
 85 173
 91 174
 94 175
 99 178
105 179
105 180
108 179
|#
