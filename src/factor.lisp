(in-package :yaft)

(sera:-> factor
         ((integer 2 #.most-positive-fixnum))
         (values list &optional))
(defun factor (n)
  "Factor N>1 into prime multipliers."
  (declare (optimize (speed 3)))
  (labels ((%factor (n &optional acc (counter 2))
             (declare (type alex:positive-fixnum n counter))
             (cond
               ((= n 1) acc)
               ((> counter (floor (sqrt n)))
                (cons n acc))
               (t
                (multiple-value-bind (q r)
                    (floor n counter)
                  (if (zerop r)
                      (%factor q (cons counter acc) 2)
                      (%factor n acc (1+ counter))))))))
    (reverse
     (%factor n))))
