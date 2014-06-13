;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: ATCephes
;;;Contents: Vicare Scheme specific functions
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
(library (cephes compat)
  (export
    define-shared-object
    define-c-function)
  (import (vicare)
    (prefix (vicare ffi) ffi.))


(define-syntax (define-c-function stx)
  (define (main stx)
    (syntax-case stx ()
      ((?ctx ?retval-type ?c-function-name (?arg-type ...))
       (with-syntax
	   ((LIBTOKEN		(datum->syntax #'?ctx 'libtoken))
	    (RETVAL-TYPE	(%external-type-id->internal-type-id #'?retval-type))
	    (FUNC-NAME		(symbol->string (syntax->datum #'?c-function-name)))
	    ((ARG-TYPES ...)	(map %external-type-id->internal-type-id
				  (syntax->list #'(?arg-type ...)))))
	 #'(define ?c-function-name
	     ((ffi.make-c-callout-maker (quote RETVAL-TYPE)
					(quote (ARG-TYPES ...)))
	      (ffi.dlsym LIBTOKEN FUNC-NAME)))))))

  (define (%external-type-id->internal-type-id type-id)
    (datum->syntax type-id
		   (%external-type-symbol->internal-type-symbol
		    (syntax->datum type-id))))

  (define (%external-type-symbol->internal-type-symbol type-sym)
    (case type-sym
      ((int)			'signed-int)
      ((signed-int)		'signed-int)
      ((signed-int*)		'pointer)
      ((unsigned-int)		'unsigned-int)
      ((unsigned-int*)		'pointer)
      ((char*)			'pointer)
      ((double)			'double)
      ((double*)		'pointer)
      ((double-complex*)	'pointer)
      ((short)			'signed-short)
      ((short*)			'pointer)
      ((long)			'signed-long)
      ((long*)			'pointer)

      ((unsigned-short*)	'pointer)
      ((unsigned-long*)		'pointer)

      ((void*)			'pointer)
      (else			type-sym)))

  (main stx))


(define-syntax (define-shared-object stx)
  (syntax-case stx ()
    ((?ctx ?libname)
     (with-syntax
	 ((LIBTOKEN	(datum->syntax #'?ctx 'libtoken)))
       #'(define LIBTOKEN
	   (ffi.open-shared-object ?libname))))
    ))


;;;; done

)

;;; end of file
;; Local Variables:
;; End:
