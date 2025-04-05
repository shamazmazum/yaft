(in-package :yaft/benchmark)

(defun benchmark ()
  (format t "Complex FFT~%")
  (loop for l from 100 to 100000 by 5000
        for array = (make-array l
                                :element-type '(complex double-float)
                                :initial-element #c(0d0 0d0))
        do
        (format t "Sequence length = ~d~%" l)
        (tb:with-timing (100)
          (yaft:fft array yaft:+forward+))))
