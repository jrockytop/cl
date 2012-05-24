;;;
;;; The common lisp magic for #! acceptance
;;;
(set-dispatch-macro-character #\# #\!
			      (lambda (s c n)
				(declare (ignore c n))
				(read-line s nil nil t)
				t))
