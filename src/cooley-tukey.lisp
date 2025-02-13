(in-package :yaft)

(declaim
 (type (complex double-float)
       +forward+
       +inverse+)
 (type alex:positive-fixnum *small-fft*))

(defconstant +forward+ #c(0d0 -1d0)
  "Used in FFT for forward transform.")

(defconstant +inverse+ #c(0d0 1d0)
  "Used in FFT for inverse transform.")

(defparameter *small-fft* 2
  "If length of input vector is less that *SMALL-FFT* a naïve O(n^2)
algorithm is applied.")

(sera:-> small-fft
         ((complex-array double-float)
          (complex double-float))
         (values (complex-array double-float) &optional))
(defun small-fft (array direction)
  "Calculate FFT using naïve O(n^2) algorithm. Direction can be
+FORWARD+ or +INVERSE+."
  (declare (optimize (speed 3)))
  (let* ((length (length array))
         (result (make-array length :element-type '(complex double-float))))
    (aops:each-index! result k
      (loop for l fixnum below length sum
           (* (aref array l)
              (exp (* direction (/ (* 2 pi) length) k l)))
         of-type (complex double-float)))
    result))

(sera:-> phase-split
         ((complex-array double-float) alex:positive-fixnum)
         (values list &optional))
(defun phase-split (array n)
  (declare (optimize (speed 3)))
  (let ((new-length (/ (length array) n)))
    (assert (integerp new-length))
    (let ((result (loop repeat n collect
                       (make-array new-length :element-type '(complex double-float))))
          (counter 0))
      (dotimes (i new-length)
        (dotimes (j n)
          (setf (aref (the (complex-array double-float)
                           (nth j result))
                      i)
                (aref array counter)
                counter (1+ counter))))
      result)))

(declaim (inline n-phase-fft))
(sera:-> n-phase-fft
         (list alex:positive-fixnum (complex double-float))
         (values (complex-array double-float) &optional))
(defun n-phase-fft (sub-ffts length direction)
  (let ((result (make-array length :element-type '(complex double-float))))
    (aops:each-index! result k
      (loop for l fixnum from 0 by 1
            for sub-fft of-type (complex-array double-float) in sub-ffts sum
            (let ((x (aref sub-fft (rem k (length sub-fft)))))
              (if (zerop l) x
                  (* (exp (* direction k l (/ (* 2 pi) length))) x)))
            of-type (complex double-float)))
    result))

(declaim (inline two-phase-fft))
(sera:-> two-phase-fft
         ((complex-array double-float)
          (complex-array double-float)
          (complex double-float))
         (values (complex-array double-float) &optional))
(defun two-phase-fft (even odd direction)
  (let* ((phase-length (length odd))
         (length (* phase-length 2))
         (result (make-array length :element-type '(complex double-float))))
    (loop for l below phase-length
          for e = (aref even l)
          for o = (* (aref odd l)
                     (exp (* direction l (/ (* 2 pi) length))))
          do
          (setf (aref result l)
                (+ e o)
                (aref result (+ phase-length l))
                (- e o)))
    result))

(sera:-> cooley-tukey-fft
         ((complex-array double-float)
          (complex double-float)
          (integer 2 #.most-positive-fixnum))
         (values (complex-array double-float) &optional))
(defun cooley-tukey-fft (array direction phases)
  (declare (optimize (speed 3)))
  (let ((length (length array)))
    (if (<= length *small-fft*)
        (small-fft array direction)
        (let ((sub-ffts (mapcar
                         (lambda (phase)
                           (fft phase direction))
                         (phase-split array phases))))
          (if (= phases 2)
              (two-phase-fft (first  sub-ffts)
                             (second sub-ffts)
                             direction)
              (n-phase-fft sub-ffts length direction))))))
