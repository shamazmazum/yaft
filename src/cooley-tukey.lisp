(in-package :yaft)

(declaim
 (type (complex double-float)
       +forward+
       +inverse+))

(defconstant +forward+ #c(0d0 -1d0)
  "Used in FFT for forward transform.")

(defconstant +inverse+ #c(0d0 1d0)
  "Used in FFT for inverse transform.")

(declaim (optimize (speed 3)))

(sera:-> small-fft
         ((complex-array double-float)
          (complex double-float))
         (values (complex-array double-float) &optional))
(defun small-fft (array direction)
  "Calculate FFT using naïve O(n^2) algorithm. Direction can be
+FORWARD+ or +INVERSE+."
  (declare (type (complex-array double-float) array)
           (type (complex double-float) direction))
  (let* ((length (length array))
         (result (make-array length :element-type '(complex double-float))))
    (aops:each-index! result k
      (loop for l fixnum below length sum
           (* (aref array l)
              (exp (* direction (/ (* 2 pi) length) k l)))
         of-type (complex double-float)))
    result))

(sera:-> phase-splice
         ((complex-array double-float)
          alex:positive-fixnum)
         (values list &optional))
(defun phase-split (array n)
  (declare (type (complex-array double-float) array)
           (type alex:positive-fixnum n))
  (let ((new-length (/ (length array) n)))
    (assert (integerp new-length))
    (let ((result (loop repeat n collect
                       (make-array new-length :element-type '(complex double-float)))))
      (dotimes (i new-length)
        (dotimes (j n)
          (setf (aref (the (complex-array double-float)
                           (nth j result))
                      i)
                (aref array (+ (* n i) j)))))
      result)))

(sera:-> fft
         ((complex-array double-float)
          (complex double-float))
         (values (complex-array double-float) &optional))
(defun fft (array direction)
  "Calculate DFT for array of (COMPLEX DOUBLE-FLOAT) values. DIRECTION can be
either +FORWARD+ or +INVERSE+. A forward DFT is unnormalized and an inverse is
multiplied by (LENGTH ARRAY)."
  (declare (type (complex-array double-float) array)
           (type (complex double-float) direction))
  (let ((factors (factor (length array))))
    (labels ((fft% (array factors)
               (declare (type (complex-array double-float) array))
               (let ((length (length array))
                     (phases (car factors)))
                 (declare (type alex:positive-fixnum length phases))
                 (cond
                   ((< length 10)
                    (small-fft array direction))
                   ((> phases 17)
                    (prime-fft array direction))
                   (t
                    (let ((sub-ffts (mapcar (lambda (phase)
                                              (fft% phase (cdr factors)))
                                            (phase-split array phases)))
                          (result (make-array length :element-type '(complex double-float))))
                      (aops:each-index! result k
                        (loop
                           for l fixnum from 0 by 1
                           for sub-fft in sub-ffts sum
                             (* (aref (the (complex-array double-float) sub-fft)
                                      (rem k (/ length phases)))
                                (if (zerop l)
                                    ;; This multiplication is not optimized out
                                    ;; but still it's faster than computing the
                                    ;; exponent.
                                    #c(1d0 0d0)
                                    (exp (* direction k l (/ (* 2 pi)
                                                             length)))))
                           of-type (complex double-float)))
                      result))))))
      (fft% array factors))))
