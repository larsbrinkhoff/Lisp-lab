(defpackage #:se.brinkhoff.macbeth-color-checker
  (:use #:cl)
  (:import-from #:se.brinkhoff.colorimetric-utils
		#:define-simple-array-constant)
  (:export #:+patch-1+ #:+patch-2+ #:+patch-3+ #:+patch-4+ #:+patch-5+
	   #:+patch-7+ #:+patch-8+ #:+patch-9+ #:+patch-10+ #:+patch-11+
	   #:+patch-12+ #:+patch-13+ #:+patch-14+ #:+patch-15+ #:+patch-16+
	   #:+patch-17+ #:+patch-18+ #:+patch-19+ #:+patch-20+ #:+patch-21+
	   #:+patch-22+ #:+patch-23+ #:+patch-24+))

(in-package #:se.brinkhoff.macbeth-color-checker)

;;; 380-780 nm

;;; Patch 1: Dark skin, 3.05 YR 3.69/3.20
(define-simple-array-constant +patch-1+ single-float
  0.05 0.05 0.06 0.06 0.07 0.07 0.07 0.07 0.06 0.06
  0.06 0.06 0.06 0.05 0.05 0.05 0.05 0.05 0.05 0.05
  0.05 0.06 0.06 0.06 0.06 0.06 0.07 0.07 0.07 0.07
  0.07 0.08 0.08 0.08 0.08 0.08 0.09 0.09 0.10 0.11
  0.12 0.12 0.13 0.13 0.14 0.14 0.15 0.15 0.15 0.16
  0.16 0.17 0.17 0.18 0.19 0.20 0.20 0.21 0.22 0.23
  0.24 0.25 0.26 0.27 0.28 0.29 0.31 0.32 0.33 0.35
  0.37 0.39 0.41 0.42 0.44 0.45 0.46 0.47 0.45 0.43
  0.42)

;;; Patch 2: Light skin, 2.2 YR 6.47/4.10
(define-simple-array-constant +patch-2+ single-float
  0.10 0.12 0.14 0.16 0.18 0.19 0.20 0.20 0.20 0.20
  0.21 0.21 0.21 0.22 0.22 0.23 0.24 0.25 0.26 0.27
  0.28 0.29 0.29 0.30 0.30 0.31 0.32 0.33 0.32 0.31
  0.30 0.29 0.29 0.30 0.30 0.30 0.30 0.30 0.31 0.33
  0.36 0.40 0.44 0.47 0.49 0.51 0.53 0.54 0.55 0.56
  0.56 0.57 0.57 0.58 0.59 0.60 0.61 0.61 0.62 0.64
  0.65 0.67 0.68 0.70 0.71 0.73 0.75 0.75 0.76 0.77
  0.78 0.79 0.79 0.79 0.79 0.79 0.80 0.80 0.77 0.75
  0.73)

;;; Patch 3: Blue sky, 4.3 PB 4.95/5.55
(define-simple-array-constant +patch-3+ single-float
  0.11 0.14 0.17 0.22 0.27 0.30 0.32 0.33 0.34 0.34
  0.34 0.34 0.34 0.33 0.33 0.33 0.32 0.32 0.31 0.30
  0.29 0.29 0.28 0.27 0.26 0.25 0.24 0.23 0.23 0.22
  0.21 0.20 0.20 0.20 0.19 0.19 0.18 0.18 0.17 0.17
  0.16 0.16 0.16 0.15 0.15 0.15 0.14 0.14 0.14 0.13
  0.13 0.13 0.12 0.12 0.12 0.12 0.11 0.11 0.11 0.11
  0.11 0.10 0.10 0.10 0.10 0.10 0.10 0.10 0.10 0.10
  0.10 0.10 0.10 0.10 0.10 0.11 0.11 0.11 0.11 0.12
  0.12)

;;; Patch 4: Foliage, 6.65 GY 4.19/4.15
(define-simple-array-constant +patch-4+ single-float
  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05
  0.05 0.05 0.05 0.06 0.06 0.06 0.06 0.06 0.06 0.07
  0.07 0.07 0.07 0.07 0.08 0.09 0.11 0.13 0.16 0.17
  0.18 0.18 0.18 0.17 0.16 0.15 0.14 0.13 0.13 0.13
  0.12 0.12 0.12 0.11 0.10 0.10 0.10 0.10 0.10 0.10
  0.10 0.10 0.10 0.10 0.10 0.10 0.10 0.10 0.10 0.11
  0.13 0.15 0.17 0.21 0.25 0.28 0.31 0.33 0.34 0.35
  0.36 0.36 0.36 0.36 0.36 0.36 0.37 0.37 0.36 0.35
  0.34)

;;; Patch 5: Blue flower, 9.65 PB 5.47/6.70
(define-simple-array-constant +patch-5+ single-float
  0.12 0.15 0.20 0.26 0.33 0.39 0.42 0.44 0.45 0.45
  0.45 0.45 0.44 0.44 0.43 0.43 0.42 0.41 0.41 0.39
  0.38 0.37 0.36 0.35 0.34 0.33 0.31 0.29 0.27 0.25
  0.23 0.22 0.21 0.21 0.21 0.21 0.20 0.20 0.20 0.20
  0.21 0.22 0.22 0.23 0.24 0.24 0.25 0.25 0.24 0.24
  0.25 0.25 0.27 0.29 0.32 0.35 0.38 0.42 0.45 0.47
  0.49 0.50 0.51 0.52 0.52 0.52 0.52 0.52 0.52 0.52
  0.52 0.52 0.52 0.52 0.51 0.51 0.52 0.52 0.50 0.49
  0.49)

;;; Patch 6: Bluish green, 2.5 BG 7/6
(define-simple-array-constant +patch-6+ single-float
  0.11 0.13 0.17 0.21 0.25 0.28 0.30 0.31 0.32 0.33
  0.34 0.34 0.35 0.36 0.38 0.39 0.41 0.43 0.46 0.49
  0.52 0.55 0.57 0.58 0.58 0.58 0.58 0.58 0.57 0.56
  0.55 0.54 0.52 0.50 0.48 0.46 0.44 0.41 0.39 0.37
  0.35 0.32 0.30 0.28 0.26 0.25 0.23 0.23 0.22 0.22
  0.22 0.21 0.21 0.21 0.21 0.21 0.21 0.22 0.22 0.23
  0.23 0.24 0.24 0.25 0.25 0.25 0.25 0.25 0.24 0.25
  0.25 0.25 0.26 0.27 0.28 0.29 0.30 0.30 0.30 0.30
  0.30)

;;; Patch 7: Orange, 5 YR 6/11
(define-simple-array-constant +patch-7+ single-float
  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05
  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05
  0.06 0.06 0.06 0.06 0.06 0.06 0.07 0.08 0.09 0.10
  0.12 0.14 0.17 0.20 0.23 0.26 0.30 0.34 0.38 0.42
  0.45 0.48 0.50 0.52 0.53 0.54 0.55 0.56 0.57 0.57
  0.58 0.58 0.59 0.59 0.60 0.60 0.60 0.61 0.61 0.61
  0.62 0.62 0.62 0.63 0.63 0.63 0.63 0.63 0.63 0.64
  0.64 0.64 0.64 0.63 0.63 0.64 0.64 0.64 0.62 0.60
  0.58)

;;; Patch 8: Purplish blue, 7.5 PB 4/10.7
(define-simple-array-constant +patch-8+ single-float
  0.10 0.12 0.15 0.19 0.23 0.27 0.29 0.31 0.32 0.34
  0.35 0.36 0.37 0.38 0.39 0.38 0.37 0.36 0.35 0.33
  0.30 0.28 0.25 0.22 0.20 0.18 0.17 0.15 0.14 0.13
  0.12 0.11 0.11 0.10 0.10 0.09 0.09 0.09 0.08 0.08
  0.08 0.09 0.09 0.09 0.09 0.09 0.09 0.08 0.08 0.09
  0.09 0.09 0.10 0.11 0.11 0.12 0.12 0.13 0.13 0.12
  0.12 0.12 0.12 0.12 0.12 0.12 0.12 0.12 0.13 0.13
  0.14 0.15 0.16 0.18 0.20 0.22 0.24 0.26 0.28 0.29
  0.32)

;;; Patch 9: Moderate red, 2.5 R 5/10
(define-simple-array-constant +patch-9+ single-float
  0.10 0.11 0.12 0.14 0.14 0.15 0.14 0.14 0.14 0.13
  0.13 0.13 0.13 0.13 0.13 0.13 0.13 0.13 0.13 0.12
  0.12 0.11 0.11 0.11 0.10 0.10 0.10 0.10 0.09 0.09
  0.09 0.09 0.09 0.10 0.10 0.11 0.11 0.11 0.11 0.13
  0.16 0.21 0.27 0.35 0.42 0.47 0.52 0.55 0.57 0.58
  0.59 0.60 0.60 0.60 0.61 0.61 0.61 0.61 0.61 0.61
  0.61 0.60 0.61 0.61 0.61 0.60 0.60 0.60 0.60 0.60
  0.60 0.60 0.59 0.59 0.58 0.58 0.59 0.58 0.57 0.55
  0.54)

;;; Patch 10: Purple, 5 P 3/7
(define-simple-array-constant +patch-10+ single-float
  0.10 0.12 0.14 0.16 0.18 0.19 0.20 0.20 0.21 0.20
  0.19 0.18 0.17 0.16 0.14 0.13 0.12 0.11 0.10 0.09
  0.09 0.08 0.08 0.07 0.07 0.06 0.06 0.06 0.06 0.06
  0.05 0.05 0.05 0.05 0.05 0.05 0.06 0.06 0.05 0.05
  0.05 0.05 0.05 0.06 0.06 0.07 0.07 0.09 0.10 0.11
  0.13 0.14 0.15 0.16 0.17 0.18 0.19 0.21 0.22 0.23
  0.25 0.27 0.28 0.30 0.32 0.34 0.36 0.37 0.39 0.41
  0.42 0.44 0.45 0.46 0.47 0.48 0.49 0.50 0.48 0.47
  0.47)

;;; Patch 11: Yellow green, 5 GY 7.08/9.1
(define-simple-array-constant +patch-11+ single-float
  0.06 0.06 0.06 0.06 0.06 0.06 0.06 0.06 0.06 0.06
  0.06 0.07 0.07 0.07 0.08 0.08 0.09 0.09 0.10 0.12
  0.14 0.16 0.19 0.22 0.27 0.33 0.38 0.44 0.48 0.52
  0.53 0.54 0.55 0.54 0.53 0.52 0.51 0.50 0.49 0.47
  0.45 0.44 0.42 0.39 0.37 0.36 0.35 0.34 0.33 0.33
  0.33 0.32 0.32 0.32 0.32 0.32 0.32 0.33 0.34 0.35
  0.35 0.36 0.37 0.38 0.38 0.38 0.38 0.38 0.37 0.37
  0.38 0.38 0.39 0.40 0.41 0.42 0.43 0.44 0.43 0.41
  0.40)

;;; Patch 12: Orange yellow, 10 YR 7/10.5
(define-simple-array-constant +patch-12+ single-float
  0.06 0.06 0.06 0.06 0.07 0.07 0.06 0.06 0.06 0.06
  0.06 0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.08 0.08
  0.09 0.10 0.11 0.12 0.12 0.13 0.15 0.17 0.19 0.23
  0.27 0.32 0.37 0.42 0.46 0.49 0.51 0.53 0.55 0.57
  0.58 0.60 0.61 0.62 0.62 0.63 0.64 0.64 0.65 0.65
  0.65 0.66 0.66 0.67 0.67 0.68 0.68 0.68 0.68 0.68
  0.69 0.69 0.69 0.69 0.70 0.70 0.70 0.70 0.70 0.70
  0.70 0.70 0.70 0.70 0.69 0.70 0.70 0.70 0.67 0.65
  0.64)

;;; Patch 13: Blue, 7.5 PB 2.90/12.75
(define-simple-array-constant +patch-13+ single-float
  0.07 0.08 0.10 0.11 0.14 0.16 0.18 0.19 0.21 0.22
  0.24 0.27 0.29 0.32 0.34 0.34 0.34 0.32 0.30 0.27
  0.24 0.21 0.17 0.14 0.12 0.10 0.09 0.07 0.07 0.06
  0.05 0.05 0.05 0.05 0.05 0.04 0.04 0.04 0.04 0.04
  0.04 0.04 0.04 0.04 0.04 0.04 0.04 0.04 0.04 0.04
  0.04 0.04 0.04 0.04 0.04 0.04 0.04 0.04 0.04 0.04
  0.04 0.04 0.05 0.05 0.05 0.05 0.05 0.05 0.06 0.06
  0.06 0.07 0.08 0.09 0.10 0.12 0.14 0.15 0.17 0.19
  0.20)

;;; Patch 14: Green, 0.1 G 5.38/9.65
(define-simple-array-constant +patch-14+ single-float
  0.06 0.06 0.06 0.06 0.06 0.06 0.06 0.06 0.06 0.06
  0.06 0.06 0.07 0.07 0.07 0.07 0.08 0.08 0.09 0.10
  0.11 0.13 0.14 0.16 0.18 0.21 0.24 0.29 0.32 0.35
  0.36 0.36 0.36 0.34 0.32 0.30 0.28 0.26 0.24 0.22
  0.20 0.18 0.16 0.14 0.12 0.11 0.10 0.09 0.09 0.09
  0.08 0.08 0.08 0.08 0.08 0.08 0.08 0.08 0.08 0.08
  0.08 0.08 0.08 0.09 0.09 0.09 0.09 0.09 0.09 0.09
  0.09 0.09 0.09 0.10 0.10 0.11 0.11 0.11 0.11 0.11
  0.11)

;;; Patch 15: Red, 5 R 4/12
(define-simple-array-constant +patch-15+ single-float
  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05
  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05
  0.05 0.05 0.04 0.04 0.04 0.04 0.04 0.04 0.04 0.04
  0.04 0.04 0.05 0.05 0.05 0.05 0.05 0.05 0.06 0.06
  0.07 0.09 0.11 0.14 0.19 0.26 0.33 0.41 0.49 0.55
  0.60 0.63 0.65 0.67 0.69 0.69 0.70 0.70 0.71 0.71
  0.72 0.72 0.72 0.73 0.73 0.73 0.73 0.73 0.73 0.73
  0.73 0.73 0.73 0.72 0.72 0.72 0.73 0.73 0.70 0.68
  0.66)

;;; Patch 16: Yellow, 5 Y 8/11.1
(define-simple-array-constant +patch-16+ single-float
  0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05
  0.05 0.05 0.05 0.05 0.06 0.06 0.06 0.07 0.08 0.09
  0.12 0.16 0.20 0.25 0.30 0.35 0.39 0.44 0.48 0.51
  0.54 0.57 0.60 0.62 0.63 0.65 0.66 0.67 0.69 0.70
  0.71 0.72 0.73 0.73 0.74 0.74 0.75 0.75 0.75 0.76
  0.76 0.77 0.77 0.77 0.78 0.78 0.78 0.78 0.78 0.78
  0.79 0.79 0.79 0.79 0.79 0.79 0.79 0.79 0.79 0.79
  0.79 0.79 0.79 0.78 0.78 0.78 0.78 0.78 0.75 0.73
  0.71)

;;; Patch 17: Magenta, 2.5 RP 5/12
(define-simple-array-constant +patch-17+ single-float
  0.12 0.14 0.18 0.23 0.28 0.32 0.34 0.35 0.36 0.36
  0.35 0.34 0.33 0.31 0.30 0.28 0.27 0.25 0.24 0.23
  0.21 0.20 0.18 0.17 0.16 0.16 0.15 0.14 0.12 0.11
  0.11 0.10 0.10 0.11 0.11 0.11 0.11 0.11 0.11 0.12
  0.14 0.17 0.20 0.24 0.28 0.33 0.39 0.46 0.52 0.58
  0.63 0.67 0.70 0.72 0.74 0.76 0.77 0.77 0.78 0.79
  0.79 0.79 0.80 0.80 0.80 0.81 0.81 0.81 0.81 0.81
  0.81 0.81 0.81 0.81 0.81 0.81 0.81 0.81 0.79 0.77
  0.75)

;;; Patch 18: Cyan, 5 B 5/8
(define-simple-array-constant +patch-18+ single-float
  0.09 0.11 0.13 0.16 0.20 0.22 0.24 0.25 0.26 0.27
  0.28 0.30 0.32 0.33 0.35 0.37 0.39 0.41 0.43 0.45
  0.46 0.46 0.46 0.45 0.43 0.41 0.39 0.37 0.34 0.31
  0.28 0.26 0.23 0.20 0.18 0.16 0.14 0.13 0.11 0.10
  0.10 0.09 0.09 0.08 0.08 0.08 0.08 0.07 0.07 0.07
  0.07 0.07 0.07 0.07 0.07 0.08 0.08 0.08 0.08 0.08
  0.08 0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.08 0.08
  0.08 0.09 0.10 0.12 0.13 0.15 0.18 0.19 0.20 0.21
  0.21)

;;; Patch 19: White, N 9.5/
(define-simple-array-constant +patch-19+ single-float
  0.15 0.19 0.25 0.32 0.41 0.54 0.67 0.77 0.84 0.87
  0.88 0.88 0.88 0.89 0.89 0.89 0.89 0.89 0.89 0.89
  0.89 0.89 0.89 0.89 0.89 0.89 0.89 0.89 0.89 0.89
  0.89 0.89 0.89 0.89 0.89 0.89 0.89 0.89 0.89 0.89
  0.89 0.89 0.89 0.89 0.89 0.89 0.89 0.89 0.89 0.89
  0.89 0.89 0.89 0.89 0.89 0.89 0.89 0.89 0.89 0.89
  0.89 0.89 0.89 0.89 0.89 0.89 0.89 0.89 0.89 0.89
  0.89 0.89 0.88 0.88 0.88 0.88 0.88 0.88 0.88 0.88
  0.88)

;;; Patch 20: Neutral 8, N 8/
(define-simple-array-constant +patch-20+ single-float
  0.15 0.18 0.24 0.30 0.37 0.46 0.53 0.56 0.58 0.58
  0.59 0.59 0.59 0.59 0.59 0.59 0.59 0.59 0.58 0.58
  0.58 0.58 0.58 0.58 0.58 0.58 0.58 0.58 0.58 0.58
  0.58 0.58 0.58 0.58 0.58 0.58 0.58 0.59 0.59 0.59
  0.59 0.59 0.59 0.59 0.59 0.59 0.59 0.59 0.59 0.58
  0.58 0.58 0.58 0.58 0.58 0.58 0.58 0.58 0.57 0.57
  0.57 0.57 0.57 0.57 0.57 0.57 0.57 0.57 0.56 0.56
  0.56 0.56 0.56 0.56 0.56 0.56 0.56 0.55 0.55 0.55
  0.55)

;;; Patch 21: Neutral 6.5, N 6.5/
(define-simple-array-constant +patch-21+ single-float
  0.14 0.17 0.21 0.25 0.29 0.32 0.35 0.35 0.36 0.36
  0.36 0.36 0.36 0.36 0.36 0.36 0.36 0.36 0.36 0.36
  0.36 0.36 0.36 0.36 0.36 0.36 0.36 0.36 0.36 0.36
  0.36 0.36 0.36 0.36 0.36 0.36 0.36 0.36 0.36 0.36
  0.36 0.36 0.36 0.36 0.36 0.36 0.36 0.36 0.36 0.36
  0.36 0.35 0.35 0.35 0.35 0.35 0.35 0.35 0.35 0.35
  0.35 0.34 0.34 0.34 0.34 0.34 0.34 0.34 0.34 0.34
  0.34 0.33 0.33 0.33 0.33 0.33 0.33 0.33 0.33 0.33
  0.33)

;;; Patch 22: Neutral 5, N 5/
(define-simple-array-constant +patch-22+ single-float
  0.11 0.13 0.15 0.17 0.18 0.19 0.20 0.20 0.20 0.20
  0.20 0.20 0.21 0.21 0.21 0.21 0.20 0.20 0.20 0.20
  0.20 0.20 0.20 0.20 0.20 0.20 0.20 0.20 0.20 0.20
  0.20 0.20 0.20 0.20 0.20 0.20 0.20 0.20 0.20 0.20
  0.21 0.21 0.21 0.21 0.20 0.20 0.20 0.20 0.20 0.20
  0.20 0.20 0.20 0.20 0.20 0.20 0.20 0.20 0.20 0.20
  0.20 0.19 0.19 0.19 0.19 0.19 0.19 0.19 0.19 0.19
  0.19 0.19 0.19 0.19 0.19 0.19 0.19 0.19 0.18 0.18
  0.18)

;;; Patch 23: Neutral 3.5, N 3.5/
(define-simple-array-constant +patch-23+ single-float
  0.07 0.08 0.08 0.09 0.09 0.09 0.09 0.09 0.09 0.09
  0.09 0.10 0.10 0.10 0.10 0.09 0.09 0.09 0.09 0.09
  0.09 0.09 0.09 0.09 0.09 0.09 0.09 0.09 0.09 0.09
  0.09 0.09 0.09 0.09 0.09 0.09 0.09 0.09 0.09 0.09
  0.09 0.09 0.09 0.09 0.09 0.09 0.09 0.09 0.09 0.09
  0.09 0.09 0.09 0.09 0.09 0.09 0.09 0.09 0.09 0.09
  0.09 0.09 0.09 0.09 0.09 0.09 0.09 0.09 0.09 0.09
  0.09 0.09 0.09 0.08 0.08 0.08 0.08 0.08 0.08 0.08
  0.08)

;;; Patch 24: Black, N 2/
(define-simple-array-constant +patch-24+ single-float
  0.03 0.03 0.03 0.03 0.04 0.04 0.04 0.04 0.04 0.04
  0.04 0.04 0.04 0.04 0.04 0.04 0.04 0.04 0.04 0.04
  0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03
  0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03
  0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03
  0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03
  0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03
  0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.03
  0.03)
