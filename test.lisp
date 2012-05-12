#!./cl 

(format t "You passed in these arguments: ~A~%" *args*)
(dolist (i *args*)
  (print i))
(format t "~%Hello from ~A~%" *program-name*)
(write-line (lisp-implementation-type))
(write-line (lisp-implementation-version))
;(print (require 'cffi))
;fail
