(in-package :yaft)

(defmacro push-to-tail (tail x)
  (let ((new-tail (gensym)))
    `(let ((,new-tail (cons ,x nil)))
       (setf (cdr ,tail) ,new-tail
             ,tail ,new-tail))))

(defgenerator primes ()
  "Return a function which generates an infinite list of prime
numbers."
  (let* ((primes (list (gensym)))
         (tail% primes))
    (loop for n from 2 by 1 do
         (let ((primep
                (notany
                 (lambda (p) (zerop (rem n p)))
                 (cdr primes))))
           (when primep
             (push-to-tail tail% n)
             (yield n))))))

(defun factor (n)
  "Factor N>1 into prime multipliers."
  (declare (type (integer 2) n))
  (let ((primes (primes)))
    (labels ((factor% (factors n p)
               (let ((p (or p (funcall primes))))
                 (cond
                   ((= n 1) factors)
                   ((> p (ceiling (sqrt n)))
                    (cons n factors))
                   ((zerop (rem n p))
                    (factor% (cons p factors)
                             (/ n p) p))
                   (t
                    (factor% factors n nil))))))
      (factor% nil n nil))))
