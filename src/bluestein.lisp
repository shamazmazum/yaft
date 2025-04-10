(in-package :yaft)

(sera:-> closest-power-of-2 (alex:positive-fixnum)
         (values alex:positive-fixnum &optional))
(declaim (inline closest-power-of-2))
(defun closest-power-of-2 (n)
  (let ((length (integer-length (1- n))))
    (ash 1 length)))

(sera:-> bluestein
         ((complex-array double-float)
          (complex double-float))
         (values (complex-array double-float) &optional))
(defun bluestein (array direction)
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
      (exp (/ (* pi direction (expt k 2)) length)))

    (map-into s1 #'* array helper-sequence)
    (map-into s2 #'conjugate helper-sequence)
    ;; And an ugly imperative-style addendum ;)
    (loop
       for i below length
       for x = (conjugate (aref helper-sequence i)) do
         (setf (aref s2 (rem (- padded-length i) padded-length)) x))

    (let ((convolution (cooley-tukey-fft!
                        (map-into 
                         s1 (lambda (s1 s2)
                              (/ (* s1 s2) padded-length))
                         (cooley-tukey-fft! s1 +forward+)
                         (cooley-tukey-fft! s2 +forward+))
                        +inverse+)))
      (map-into helper-sequence #'*
                convolution helper-sequence))))
