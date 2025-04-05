(in-package :yaft)

;; This declaration is the same is in early-defs
(sera:-> fft
         ((complex-array double-float)
          (complex double-float))
         (values (complex-array double-float) &optional))
(defun fft (array direction)
  "Calculate DFT for array of (COMPLEX DOUBLE-FLOAT) values. DIRECTION can be
either +FORWARD+ or +INVERSE+. A forward DFT is unnormalized and an inverse is
multiplied by (LENGTH ARRAY)."
  (declare (optimize (speed 3)))
  (if (length-power-of-2-p array)
      (cooley-tukey-fft/inplace array direction)
      (bluestein array direction)))
