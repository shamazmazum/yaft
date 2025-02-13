(in-package :yaft)

(sera:-> divisor
         ((integer 2 #.most-positive-fixnum))
         (values (integer 2 #.most-positive-fixnum) &optional))
(defun divisor (n)
  "Find the smallest divisor D>1 of an iteger N>1"
  (declare (optimize (speed 3)))
  (let ((limit (isqrt n)))
    (labels ((%go (d)
               (declare (type fixnum d))
               (cond
                 ((= d limit) n)
                 ((zerop (rem n d)) d)
                 (t (%go (1+ d))))))
      (%go 2))))
