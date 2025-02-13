(in-package :yaft)

(deftype complex-array (type)
  "Alias for (SIMPLE-ARRAY (COMPLEX TYPE) (*))"
  `(simple-array (complex ,type) (*)))

(deftype real-array (type)
  "Alias for (SIMPLE-ARRAY TYPE (*))"
  `(simple-array ,type (*)))

(define-condition yaft-error (simple-error)
  ()
  (:documentation "General yaft error"))

;; This is needed here because it's called before it's defined.
(sera:-> fft
         ((complex-array double-float)
          (complex double-float))
         (values (complex-array double-float) &optional))
