;;;
;;;Part of: ATCephes
;;;Contents: convert table into FFI stuff
;;;Date: Fri Jun 13, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;The author hereby  grant permission to use, copy, modify,  distribute, and license
;;;this  software and  its  documentation  for any  purpose,  provided that  existing
;;;copyright notices  are retained  in all  copies and that  this notice  is included
;;;verbatim in  any distributions. No written  agreement, license, or royalty  fee is
;;;required for  any of the authorized  uses.  Modifications to this  software may be
;;;copyrighted by  their authors and  need not  follow the licensing  terms described
;;;here, provided that the new terms are  clearly indicated on the first page of each
;;;file where they apply.
;;;
;;;IN NO EVENT  SHALL THE AUTHOR OR  DISTRIBUTORS BE LIABLE TO ANY  PARTY FOR DIRECT,
;;;INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES  ARISING OUT OF THE USE OF
;;;THIS SOFTWARE, ITS  DOCUMENTATION, OR ANY DERIVATIVES THEREOF, EVEN  IF THE AUTHOR
;;;HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;THE AUTHOR AND  DISTRIBUTORS SPECIFICALLY DISCLAIM ANY  WARRANTIES, INCLUDING, BUT
;;;NOT  LIMITED  TO,  THE  IMPLIED  WARRANTIES  OF  MERCHANTABILITY,  FITNESS  FOR  A
;;;PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE IS PROVIDED ON AN "AS IS"
;;;BASIS, AND THE AUTHOR AND DISTRIBUTORS  HAVE NO OBLIGATION TO PROVIDE MAINTENANCE,
;;;SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
;;;


#!r6rs
(import (rnrs (6))
  ;;(vicare)
  )

(define sexp
  ;; We expect SEXP to have the format:
  ;;
  ;;   (foreign-library
  ;;     (name "cephes")
  ;;     (version
  ;;       (major	?major-number)
  ;;       (minor	?minor-number)
  ;;       (patch	?patch-number))
  ;;     (constants
  ;;       (?const-name	?const-value)
  ;;       ...)
  ;;     (type-aliases
  ;;       (?alias-name	?type-name)
  ;;       ...)
  ;;     (functions ?func-spec ...))
  ;;
  (let* ((P (open-input-file "table.scm"))
	 (D (read P)))
    (close-port P)
    D))

(define name
  ;;A symbol representing the library identifier name.
  ;;
  (cadr (list-ref sexp 1)))

(define libname
  ;;A string representing the Unix-style shared object file name.
  ;;
  (string-append "lib" name ".so"))

(define version-spec
  ;;A list of version numbers.
  ;;
  (map cadr (cdr (list-ref sexp 2))))

(define constant-spec*
  ;;A list of constants.
  ;;
  (cdr (list-ref sexp 3)))

(define constant-name*
  (map symbol->string (map car constant-spec*)))

(define func-spec*
  ;;We expect each FUNC-SPEC to have the format:
  ;;
  ;;   (?retval ?func-name (?arg-type ?arg-name ...))
  ;;
  (cdr (list-ref sexp 5)))

(define func-name*
  ;;List of strings representing the C function names.
  ;;
  (map (lambda (func-spec)
	 (symbol->string (list-ref func-spec 1)))
    func-spec*))

;;; --------------------------------------------------------------------

(display (string-append "(library (" name " "))
(display version-spec)
(display ")\n")
(display "  (export")
#;(map (lambda (const-name)
       (display (string-append "\n    " const-name "")))
  constant-name*)
(map (lambda (func-name)
       (display (string-append "\n    " func-name "")))
  func-name*)
(display ")\n")
(display "  (import (rnrs (6)) (cephes compat))\n")
(display (string-append "  (define-shared-object \"" libname "\")\n"))

#;(map (lambda (const-spec)
       (display "  (define ")
       (display (car const-spec))
       (display " ")
       (display (cadr const-spec))
       (display ")\n"))
  constant-spec*)

(map (lambda (func-spec)
       ;;We expect FUNC-SPEC to have the format:
       ;;
       ;;   (?retval ?func-name (?arg-type ?arg-name ...))
       ;;
       (let ((retval     (list-ref func-spec 0))
	     (func-name  (list-ref func-spec 1))
	     (arg*       (list-ref func-spec 2)))
	 (display (string-append "  (define-c-function "
				 (symbol->string retval) " "
				 (symbol->string func-name)
				 " ("))
	 (if (equal? arg* '(void))
	     (display 'void)
	   ;;Here we know ARG* holds an even number of items.
	   (let loop ((arg* arg*))
	     (unless (null? arg*)
	       (display (symbol->string (car arg*)))
	       (display " ")
	       (loop (cddr arg*)))))
	 (display "))\n")))
  func-spec*)

(display "  #| end of library |#)\n\n;;; end of file\n")
(flush-output-port (current-output-port))

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
