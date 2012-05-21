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
;;; parse-args
;;;
;;; This macro parses arguments from the *args* global variable.  It
;;; takes a list of options to parse in the following format:
;;;
;;; (short-option-string & option argument, long-option-string, description)
;;;
;;; Example parse-args options:
;;; (parse-args (("v"            "verbose" "The verbose flag")
;;;              ("f <filename>" "file"    "The file option")
;;;              ("q"            "quit"    "Quit"))
;;;
;;; The macro will create variables based on the long-option string.
;;; For instance in the example, the variables 'flag-verbose',
;;; 'flag-file', and 'flag-quit' will be created.  Options that don't
;;; take arguments will be set to the number of times the option
;;; appears or to nil if the option wasn't set.  Options that take
;;; arguments will be set to the corresponding last argument that 
;;; appears or nil if the option did not appear.
;;;
;;; The non-flag arguments are in the variable 'parsed-args'
;;;
;;; A help flag and help message are automatically generated.
;;;
(defmacro parse-args (opts &body body)
  ;; Too lazy to type help options... add it automatically...
  ;; unless somebody already added it on their own...
  (pushnew '("h" "help" "Display help") 
	   opts :test (lambda (x y)
			(string= (car x) (car y))))
  (labels (
           ;;;
           ;;; Return the flag-name for an option entry
           ;;;
	   (flag-name (entry)
	     (intern (string-upcase (concatenate 'string "flag-" (second entry)))))
	   
           ;;;
           ;;; Return a list of all flag-names given the list of options
           ;;;
	   (flag-names (opts)
	     (let (list)
	       (dolist (i opts)
		 (push (flag-name i) list))
	       (push 'parsed-args list)
	       (pushnew 'flag-help list)
	       list))
	   
	   ;;;
	   ;;; Generate code to setq the flag variables
           ;;;
	   (set-strings (opts var)
	     (let ((l '(case (second f))))
	       (dolist (i opts)
		 (setf l (append l (list (list (second i) (list 'setq (flag-name i) var))))))
	       l)))

    `(let ,(flag-names opts)
       (labels (
                ;;;
                ;;; This function takes a string and returns whether the
                ;;; string is an arg, long-option, short-option, or 
                ;;; expandable-short-options
                ;;;
		(element-type (element)
		  (if (or (< (length element) 2) (string= "--" element))
		      'arg
		      (case (count #\- element :end 2)
			(0 'arg)
			(1 (if (> (length element) 2)
			       'expandable-short-options
			       'short-option))
			(2 'long-option))))
		
                ;;;
                ;;; Return the short option given the option entry
                ;;;
		(entry-to-short-option (x)
		  (concatenate 'string "-" (subseq (first x) 0 1)))
		
                ;;;
                ;;; Return the long option given the option entry
                ;;;
		(entry-to-long-option (x)
		  (concatenate 'string "--" (second x)))
		
                ;;;
                ;;; Return t if the entry takes an optional argument, otherwise nil
                ;;;
		(entry-takes-arg (entry)
		  (> (length (first entry)) 1))
		
                ;;;
                ;;; Find option
                ;;;
		(find-option (opts-list option type)
		  (let ((test (if (eq type 'short-option)
				  (lambda (x y) 
				    (string= x (entry-to-short-option y)))
				  (lambda (x y) 
				    (string= x (entry-to-long-option y))))))
		    (find option opts-list :test test)))
		
                ;;;
                ;;; Given an option, return nil or t depending on if the
                ;;; given option takes an argument or not
                ;;;
		(option-takes-arg (opts-list option type)
		  (entry-takes-arg (find-option opts-list option type)))
		
                ;;;
                ;;; This function expands a short option element into a
                ;;; list of short options.  If a short option takes an
                ;;; argument, the rest of the element will be its
                ;;; argument. ie: for an option -f that takes an argument,
                ;;; "-vvfarg" will be expanded to ("-v" "-v" "-f" "arg")
                ;;;
		(expand-short-options (opts element)
		  (labels ((expand (opts element)
			     (let (list)
			       (when (> (length element) 1)
				 (let ((option (concatenate 'string "-" 
							    (string (elt element 1))))
				       (rest (subseq element 2)))
				   (push option list)
				   (if (option-takes-arg opts option 'short-option)
				       (if (> (length rest) 0)
					   (push rest list)
					   list)
				       (nconc (expand opts (concatenate 'string 
									"-" rest)) list)))))))
		    (reverse (expand opts element))))
		
                ;;;
                ;;; Set the option flags given the options and the parsed flags
                ;;;
		(set-flags (opts flags)
		  (dolist (f opts)
		    (if (entry-takes-arg f)
			(let ((pos (position f flags :from-end t 
					     :test (lambda (x y) 
						     (or (string= y (entry-to-short-option x))
							 (string= y (entry-to-long-option x)))))))
			  (when pos
			    ,(set-strings opts '(elt flags (1+ pos)))))
			(let ((num (count f flags 
					  :test (lambda (x y)
						  (or (string= y (entry-to-short-option x))
						      (string= y (entry-to-long-option x)))))))
			  (when (> num 0)
			    ,(set-strings opts 'num))))))
		
                ;;;
                ;;; Help usage
                ;;;
		(help (opts)
		  (let ((list (sort (copy-list opts) (lambda (x y)
						       (string< (car x) (car y))))))
		    (dolist (i list)
		      (format *error-output* "~a, ~a ~35t~a~&" (entry-to-long-option i)
			      (concatenate 'string "-" (first i)) (third i))))
		  (quit))
		
                ;;;
                ;;; Parse the arguments
                ;;; 
		(parse-args-function (opts args)
		  (labels ((normalize (opts args)
			     (let (list)
			       (when args
				 (let* ((element (first args))
					(rest (rest args))
					(type (element-type element)))
				   (cond 
				     ((eq type 'expandable-short-options)
				      (normalize opts 
						 (append (expand-short-options opts element) rest)))
				     
				     ((eq type 'arg)
				      (setf parsed-args args)
				      nil)
				     
				     (t ;; Options
				      (unless (find-option opts element type)
					(format *error-output* "invalid option -- ~A~&" 
						(subseq element 1))
					(error 'die))
				      
				      (if (option-takes-arg opts element type)
					  (progn
					    (push element list)
					    (unless rest
					      (format *error-output* 
						      "option requires an argument -- ~A~&" 
						      (subseq element 1))
					      (error 'die))
					    (push (first rest) list)
					    (append (normalize opts (rest rest)) list))
					  (progn
					    (push element list)
					    (append (normalize opts rest) list))))))))))
		    
		    (set-flags opts (reverse (normalize opts args)))
		    (if flag-help
			(help opts)))))
	 (handler-case
	     (parse-args-function ',opts *args*)
	   (error (e)
	     (declare (ignore e))
	     (help ',opts))))
       ,@body)))
