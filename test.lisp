#!./cl 

(format t "You passed in these arguments: ~A~%" *args*)
(dolist (i *args*)
  (print i))
(format t "~%Hello from ~A~%" *program-name*)
(write-line (lisp-implementation-type))
(write-line (lisp-implementation-version))
;(print (require 'cffi))
;fail

(parse-args (("v"        "verbose" "Set verbose flag")
	     ("f <FILE>" "file"    "Specify a file argument"))
  (format t "flag-help = ~A~%" flag-help)
  (format t "flag-verbose = ~A~%" flag-verbose)
  (format t "flag-file = ~A~%" flag-file)
  (format t "parsed-args = ~S~%" parsed-args))
