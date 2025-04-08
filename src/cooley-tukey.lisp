(in-package :yaft)

(declaim (type (complex double-float)
               +forward+
               +inverse+))

(defconstant +forward+ #c(0d0 -1d0)
  "Used in FFT for forward transform.")

(defconstant +inverse+ #c(0d0 1d0)
  "Used in FFT for inverse transform.")

(declaim (inline length-power-of-2-p))
(defun length-power-of-2-p (array)
  (let ((length (length array)))
    (zerop (logand length (1- length)))))

;; Requirement: Array length is a power of 2
(sera:-> reverse-bits (alex:positive-fixnum alex:non-negative-fixnum)
         (values alex:non-negative-fixnum &optional))
(declaim (inline reverse-bits))
(defun reverse-bits (length n)
  (loop with length = (integer-length (1- length))
        for i below length sum
        (ash (ldb (byte 1 i) n) (- length i 1))
        fixnum))

;; Requirement: Array length is a power of 2
;; SRC and DST may be the same array
(sera:-> reorder-input! ((complex-array double-float)
                         (complex-array double-float))
         (values (complex-array double-float) &optional))
(defun reorder-input! (src dst)
  (declare (optimize (speed 3)))
  (let ((length (length src)))
    (assert (= (length dst) length))
    (loop for i below length
          for j = (reverse-bits length i)
          when (<= i j) do
          (psetf (aref dst i)
                 (aref src j)
                 (aref dst j)
                 (aref src i))))
    dst)

(sera:-> reorder-input ((complex-array double-float))
         (values (complex-array double-float) &optional))
(declaim (inline reorder-input))
(defun reorder-input (src)
  (let ((dst (make-array (length src) :element-type '(complex double-float))))
    (reorder-input! src dst)))

;; Collect a list of "group ωs", that is ω^{2^{steps-k-1}} for k = 0,1,…,steps-1
(sera:-> ωs (alex:positive-fixnum
             alex:positive-fixnum
             (complex double-float))
         (values list &optional))
(defun ωs (sequence-length steps direction)
  (declare (optimize (speed 3)))
  (labels ((%go (acc k)
             (declare (type fixnum k))
             (if (zerop k) acc
                 (let ((ω (car acc)))
                   (declare (type (complex double-float) ω))
                   (%go (cons (expt ω 2) acc)
                        (1- k))))))
    (%go (list (exp (/ (* 2 direction pi) sequence-length))) (1- steps))))

;; Requirement: Array length is a power of 2
(sera:-> %fft! ((complex-array double-float)
                (complex double-float))
         (values (complex-array double-float) &optional))
(defun %fft! (array direction)
  (declare (optimize (speed 3)))
  (let* ((length (length array))
         (steps  (integer-length (1- length)))
         (ωs (ωs length steps direction)))
    (loop for s below steps
          for pair-offset  = (ash 1 s)
          for ngroups = (ash 1 (- steps s 1))
          for group-size = pair-offset ;; number of evens
          for m of-type (complex double-float) in ωs do
          (loop for i below ngroups
                for group-offset fixnum from 0 by (* group-size 2) do
                (loop for j below group-size
                      with %m of-type (complex double-float) = #c(1d0 0d0)
                      for even-idx = (+ group-offset j)
                      for odd-idx  = (+ even-idx pair-offset)
                      for even = (aref array even-idx)
                      for odd  = (aref array odd-idx)
                      for %odd = (* odd %m) do
                      (setf (aref array even-idx)
                            (+ even %odd)
                            (aref array odd-idx)
                            (- even %odd)
                            %m (* %m m)))))
    array))

(sera:-> cooley-tukey-fft
         ((complex-array double-float)
          (complex double-float))
         (values (complex-array double-float) &optional))
(defun cooley-tukey-fft (array direction)
  (declare (optimize (speed 3)))
  (assert (length-power-of-2-p array))
  (%fft! (reorder-input array) direction))

(sera:-> cooley-tukey-fft!
         ((complex-array double-float)
          (complex double-float))
         (values (complex-array double-float) &optional))
(defun cooley-tukey-fft! (array direction)
  (declare (optimize (speed 3)))
  (assert (length-power-of-2-p array))
  (%fft! (reorder-input! array array) direction))
