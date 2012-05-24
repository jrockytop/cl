;;;;
;;;; Copyright (c) 2012, Jason Wade Cox <jason@coxmountain.com>
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;   Redistributions of source code must retain the above copyright
;;;;   notice, this list of conditions and the following disclaimer.
;;;;   
;;;;   Redistributions in binary form must reproduce the above copyright
;;;;   notice, this list of conditions and the following disclaimer in
;;;;   the documentation and/or other materials provided with the
;;;;   distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;;;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;;;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;;;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;;;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;;;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;;;; OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;

;;;
;;; Reads a file and returns its contents as a list
;;;
(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read stream nil)
          while line
          collect line)))

;;;
;;; This function returns t if the symbol should be included in the
;;; initialization code and not in the main function definition
;;;
(defun symbol-goes-in-init? (x)
  (cond 
    ((listp x) 
     (case (car x)
       (dolist (every #'identity (mapcar #'symbol-goes-in-init? (cddr x))))
       (load t)
       (require t)
       #+asdf (asdf:oos t)
       #+asdf2 (asdf:load-system t)
       #+quicklisp (ql:quickload t)
       (otherwise nil)))
    (t t)))

;;;
;;; Given a list containing code from a lisp script file, return the
;;; initialization code and the code to put into the main function
;;; definition.
;;;
(defun separate-initialization-and-main-code (list)
  (let (init main)
    (dolist (i list)
      (if main
	  (push i main)
	  (if (symbol-goes-in-init? i)
	      (push i init)
	      (push i main))))
    (values (reverse init) (reverse main))))

;;;
;;; Return a list containing code to build an executable program.  The
;;; program is given in <executable-filename> and the main function is
;;; given in <main-name>
;;;
(defun build-line (executable-filename main-name)
  #+sbcl
  (list 'sb-ext:save-lisp-and-die executable-filename :toplevel 
	(list 'quote main-name) :executable t)

  #+ccl
  (list 'ccl:save-application executable-filename :toplevel-function 
	(list 'quote main-name ) :error-handler :quit :prepend-kernel t)

  #+clisp
  (list 'ext:saveinitmem executable-filename :executable t :quiet t :norc t
	:init-function (list 'function main-name))

  #+ecl
  (list 'c:build-program executable-filename :epilogue-code 
	(list 'quote main-name))
)

;;;
;;; This function creates a buildable lisp file from the lisp
;;; <script-name> The name of the executable that the buildable lisp
;;; file will create will be named <executable-name>
;;;
(defun create-build-file (script-name executable-name)
  (let ((main-name (intern (string (gensym "CL-MAIN-"))))
	(build-filename (concatenate 'string script-name ".CL-MAIN")))
    (multiple-value-bind (init main) (separate-initialization-and-main-code 
				      (read-file script-name))
      (with-open-file (f build-filename :direction :output)
	(setf *print-pretty* t)
	(print (append '(progn) (declare-args) init) f)
	(print (append (list 'defun main-name nil) (setup-args) 
		       main '((quit))) f)
	;(print (list 'delete-file build-filename) f)
	(print (build-line executable-name main-name) f)
	(print '(quit) f)))))
