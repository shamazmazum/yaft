(defun do-all()
  (ql:quickload :yaft/tests)
  (uiop:quit
   (if (uiop:call-function "yaft-tests:run-tests")
       0 1)))

(do-all)
