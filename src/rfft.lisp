(in-package :yaft)

(sera:-> rfft
         ((real-array double-float))
         (values (complex-array double-float) &optional))
(defun rfft (array)
  "Compute FFT of real-values input array of type (SIMPLE-ARRAY
DOUBLE-FLOAT (*)). Currently only arrays of even length are
supported."
  (declare (optimize (speed 3)))
  (when (oddp (length array))
    (error 'yaft-error
           :format-control "Only even input length is supported: ~d"
           :format-arguments (list (length array))))

  (let* ((half (/ (length array) 2))
         (complex-input (make-array half      :element-type '(complex double-float)))
         (result        (make-array (1+ half) :element-type '(complex double-float))))
    (aops:each-index! complex-input k
      (complex (aref array (+ 0 (* 2 k)))
               (aref array (+ 1 (* 2 k)))))
    (let ((fft (fft complex-input +forward+)))
      (flet ((fft-value (idx1 idx2 m)
               (/ (+ (* (- 1 m) (aref fft idx1))
                     (* (+ 1 m) (conjugate (aref fft idx2))))
                  2)))
        (setf (aref result 0)
              (fft-value 0 0 #c(0d0 1d0)))
        (loop for k fixnum from 1 below half do
              (setf (aref result k)
                    (fft-value k (- half k)
                               (* #c(0d0 1d0)
                                  (exp (* #c(0d0 -1d0) k
                                          (/ pi half)))))))
        (setf (aref result half)
              (fft-value 0 0 #c(0d0 -1d0)))))
    result))

(sera:-> irfft
         ((complex-array double-float)
          alex:positive-fixnum)
         (values (real-array double-float) &optional))
(defun irfft (array length)
  "Compute an inverse of RFFT function multiplied by LENGTH. LENGTH is
a length of the original array."
  (declare (optimize (speed 3)))
  (when (oddp length)
    (error 'yaft-error
           :format-control "Only even input length is supported: ~d"
           :format-arguments (list length)))

  (when (/= length (* (1- (length array)) 2))
    (error 'yaft-error
           :format-control "Incompatible sizes: ~d and ~d"
           :format-arguments (list (length array) length)))

  (let* ((half (1- (length array)))
         (ifft-input (make-array half :element-type '(complex double-float))))
    (aops:each-index! ifft-input k
      (let ((mul (* #c(0d0 -1d0) (exp (* #c(0d0 1d0) k (/ pi half))))))
        (+ (* (aref array k)
              (- 1 mul))
           (* (conjugate (aref array (- half k)))
              (+ 1 mul)))))
    (let ((fft (fft ifft-input +inverse+))
          (result (make-array length :element-type 'double-float)))
      (loop for i fixnum below half do
           (setf (aref result (+ (* i 2) 0))
                 (realpart (aref fft i))
                 (aref result (+ (* i 2) 1))
                 (imagpart (aref fft i))))
      result)))
