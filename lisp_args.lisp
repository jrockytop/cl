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
;;; Different implementations args taken from Speely
;;; http://tinyurl.com/bmnrww3
;;; 
;;; With help from Francois-Rene Rideau
;;; http://tinyurl.com/cli-args
;;;

(defun declare-args()
  `((defvar *args* nil)
    (defvar *program-name* nil)))

(defun setup-args()
  '
#+clisp ((if *args*
	    (setf *program-name* (elt (ext:argv) (1- (search *args* (ext:argv) 
							     :test #'equal))))
	    (setf *program-name* (elt (ext:argv) (1- (length (ext:argv)))))))
#+sbcl ((setf *args* sb-ext:*posix-argv*) 
	(setf *program-name* (pop *args*)))
#+clozure ((setf *args* (ccl::command-line-arguments)) 
	   (setf *program-name* (pop *args*)))
;#+gcl si:*command-args*
#+ecl ((setf *args* (loop for i from 0 below (si:argc) 
		       collect (si:argv i))))
#+cmu extensions:*command-line-strings*
;#+allegro (sys:command-line-arguments)
;#+lispworks sys:*line-arguments-list*
)
