(in-package :yaft)

(sera:-> closest-power-of-2 (alex:positive-fixnum)
         (values alex:positive-fixnum &optional))
(declaim (inline closest-power-of-2))
(defun closest-power-of-2 (n)
  (let ((length (integer-length (1- n))))
    (ash 1 length)))

(sera:-> bluestein-fft
         ((complex-array double-float))
         (values (complex-array double-float) &optional))
(defun bluestein-fft (array)
  (declare (optimize (speed 3)))
  (let* ((length (length array))
         (padded-length (closest-power-of-2 (1- (* length 2))))
         (helper-sequence (make-array length :element-type '(complex double-float)))
         (s1 (make-array padded-length
                         :element-type '(complex double-float)
                         :initial-element #c(0d0 0d0)))
         (s2 (make-array padded-length
                         :element-type '(complex double-float)
                         :initial-element #c(0d0 0d0))))

    (aops:each-index! helper-sequence k
      (exp (- (/ (* pi #c(0d0 1d0) (expt k 2)) length))))

    (map-into s1 #'* array helper-sequence)
    (map-into s2 #'conjugate helper-sequence)
    ;; And an ugly imperative-style addendum ;)
    (loop
       for i below length
       for x = (conjugate (aref helper-sequence i)) do
         (setf (aref s2 (rem (- padded-length i) padded-length)) x))

    (let ((convolution (fft (map '(vector (complex double-float))
                                 (lambda (s1 s2)
                                   (/ (* s1 s2) padded-length))
                                 (fft s1 +forward+)
                                 (fft s2 +forward+))
                            +inverse+)))
      (map '(vector (complex double-float)) #'*
           convolution helper-sequence))))

(sera:-> bluestein-ifft
         ((complex-array double-float))
         (values (complex-array double-float) &optional))
(defun bluestein-ifft (array)
  (declare (optimize (speed 3)))
  ;; Generic IFFT formula
  (let ((fft (bluestein-fft
              (map '(vector (complex double-float))
                   #'conjugate array))))
    (map '(vector (complex double-float))
         #'conjugate fft)))

(sera:-> bluestein
         ((complex-array double-float)
          (complex double-float))
         (values (complex-array double-float) &optional))
(defun bluestein (array direction)
  (declare (optimize (speed 3)))
  (cond
    ((= direction +forward+)
     (bluestein-fft  array))
    ((= direction +inverse+)
     (bluestein-ifft array))
    (t (error 'yaft-error
              :format-control "Invalid direction: ~f"
              :format-arguments (list direction)))))
