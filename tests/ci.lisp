(defun do-all()
  (handler-case
      (asdf:load-system :yaft/tests)
    (error ()
      (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "yaft-tests:run-tests")
       0 1)))

(do-all)
