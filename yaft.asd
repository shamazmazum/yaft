(defsystem :yaft
    :name :yaft
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "Yet another FFT library"
    :license "2-clause BSD"
    :serial t
    :pathname "src"
    :components ((:file "package")
                 (:file "early-defs")
                 (:file "cooley-tukey")
                 (:file "bluestein")
                 (:file "fft")
                 (:file "rfft"))
    :depends-on (:alexandria
                 :serapeum
                 :array-operations)
    :in-order-to ((test-op (load-op "yaft/tests")))
    :perform (test-op (op system)
                      (declare (ignore op system))
                      (uiop:symbol-call :yaft-tests '#:run-tests)))

(defsystem :yaft/tests
    :name :yaft/tests
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :pathname "tests"
    :serial t
    :components ((:file "package")
                 (:file "tests"))
    :depends-on (:yaft :fiveam))

(defsystem :yaft/benchmark
    :name :yaft/benchmark
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :pathname "benchmark"
    :serial t
    :components ((:file "package")
                 (:file "benchmark"))
    :depends-on (:yaft :trivial-benchmark))
