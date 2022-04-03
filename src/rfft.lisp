(in-package :yaft)

(declaim (optimize (speed 3)))

(sera:-> rfft
         ((real-array double-float))
         (values (complex-array double-float) &optional))
(defun rfft (array)
  "Compute FFT of real-values input array of type (SIMPLE-ARRAY
DOUBLE-FLOAT (*)). Currently only arrays of even length are
supported."
  (declare (type (real-array double-float) array))

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
      (loop
         for k below (1+ half)
         for mul = (* #c(0d0 1d0)
                      (if (< k half) 1 -1)
                      (exp (* #c(0d0 -1d0)
                              (rem k half)
                              (/ pi half))))
         do
           (setf (aref result k)
                 (/ (+ (* (- 1 mul) (aref fft (rem k half)))
                       (* (+ 1 mul) (conjugate (aref fft (rem (- half k) half)))))
                    2))))
    result))

(sera:-> irfft
         ((complex-array double-float)
          alex:positive-fixnum)
         (values (real-array double-float) &optional))
(defun irfft (array length)
  "Compute an inverse of RFFT function multiplied by LENGTH. LENGTH is
a length of the original array."
  (declare (type (complex-array double-float) array)
           (type alex:positive-fixnum length))

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
