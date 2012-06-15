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
;;; Setup outfile variable for the executable name
;;;
(if (fboundp 'main)
  #+sbcl
  (sb-ext:save-lisp-and-die outfile :toplevel 'cl-main-wrapper :executable t)
  #+ccl
  (ccl:save-application outfile :toplevel-function 
			'cl-main-wrapper :error-handler :quit :prepend-kernel t)
  #+clisp
  (ext:saveinitmem outfile :executable t :quiet t :norc t
		   :init-function (function cl-main-wrapper))
  #+ecl
  (c:build-program outfile :epilogue-code 'cl-main-wrapper)

  (error "die no main"))
