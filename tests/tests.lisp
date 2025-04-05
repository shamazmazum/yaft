(in-package :yaft-tests)

(def-suite yaft :description "Test yaft")

(defun run-tests ()
  (let ((status (run 'yaft)))
    (explain! status)
    (results-status status)))

(defun approx-= (x y)
  (< (abs (- x y)) 1d-6))

(sera:-> small-fft
         ((yaft::complex-array double-float)
          (complex double-float))
         (values (yaft::complex-array double-float) &optional))
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

(in-suite yaft)

(test fft-vs-naïve
  (loop repeat 10
        for array = (make-array 4000
                                :element-type '(complex double-float)
                                :initial-contents (loop repeat 4000 collect
                                                        (complex (random 1d0)
                                                                 (random 1d0))))
        for fft = (yaft:fft array yaft:+forward+)
        for dft = (small-fft array yaft:+forward+)
        do (is-true (every #'approx-= fft dft))))

(test fft-inverse
  (loop repeat 10
        for array = (make-array 5000
                                :element-type '(complex double-float)
                                :initial-contents (loop repeat 5000 collect
                                                        (complex (random 1d0)
                                                                 (random 1d0))))
        for array2 = (yaft:fft (yaft:fft array yaft:+forward+) yaft:+inverse+)
        do (is-true (every (sera:hook2 #'approx-= (alex:rcurry #'/ (length array)))
                           array array2))))

(test rfft
  (loop repeat 10
        for array = (make-array 7000
                                :element-type 'double-float
                                :initial-contents (loop repeat 7000 collect (random 1d0)))
        do (is-true (every #'approx-=
                           (yaft:rfft array)
                           (yaft:fft (map '(vector (complex double-float))
                                          #'complex array)
                                     yaft:+forward+)))))

(test rfft-inverse
  (loop repeat 10
        for array = (make-array 7000
                                :element-type 'double-float
                                :initial-contents (loop repeat 7000 collect (random 1d0)))
        for array2 = (yaft:irfft (yaft:rfft array) 7000)
        do (is-true (every (sera:hook2 #'approx-= (alex:rcurry #'/ (length array)))
                           array array2))))

(test fft-prime
  (loop repeat 10
        for array = (make-array 97
                                :element-type '(complex double-float)
                                :initial-contents (loop repeat 97 collect (complex (random 1d0))))
        for fft = (yaft:fft array yaft:+forward+)
        for dft = (small-fft array yaft:+forward+)
        do (is-true (every #'approx-= fft dft))))


(test fft-prime-inverse
  (loop repeat 10
     for array = (make-array 8191
                             :element-type '(complex double-float)
                             :initial-contents (loop repeat 8191 collect (complex (random 1d0))))
     for array2 = (yaft:fft (yaft:fft array yaft:+forward+) yaft:+inverse+)
     do (is-true (every (sera:hook2 #'approx-= (alex:rcurry #'/ (length array)))
                        array array2))))

;; Test evaluation with different array lengths
(test evaluates-ok-fft
  (loop for n below 3000
        for a = (make-array (+ n 2)
                            :element-type '(complex double-float)
                            :initial-contents
                            (loop repeat (+ n 2) collect
                                  (complex (random 1d0)
                                           (random 1d0))))
        for forward  = (yaft:fft a       yaft:+forward+)
        for backward = (yaft:fft forward yaft:+inverse+) do
        (is-true (every (lambda (x) (< x 1d-8))
                        (map '(vector double-float)
                             (lambda (x y)
                               (abs (- x (/ y (+ n 2)))))
                             a backward)))))

(test evaluates-ok-rfft
  (loop for n from 4 to 3000 by 2
        for a = (make-array n
                            :element-type 'double-float
                            :initial-contents
                            (loop repeat n collect (random 1d0)))
        for forward  = (yaft:rfft a)
        for backward = (yaft:irfft forward n) do
        (is-true (every (lambda (x) (< x 1d-8))
                        (map '(vector double-float)
                             (lambda (x y)
                               (abs (- x (/ y n))))
                             a backward)))))
