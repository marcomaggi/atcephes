;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: ATCephes
;;;Contents: test file for Vicare Scheme template binding
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


#!vicare
(import (vicare)
  (cephes)
  (prefix (vicare platform words) words.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Cephes\n")


;;;; generic helpers

(define-constant sizeof-double
  words.SIZEOF_DOUBLE)

(define-constant sizeof-double-complex
  (* 2 sizeof-double))

;;; --------------------------------------------------------------------

(define-constant EPSILON
  1e-6)

(define* (flonum-vector=? {O1 vector?} {O2 vector?})
  (let loop ((i 0))
    (or (fx=? i (vector-length O1))
	(and (let ((X (vector-ref O1 i))
		   (Y (vector-ref O2 i)))
	       (< (magnitude (- X Y)) EPSILON))
	     (loop (fxadd1 i))))))

(define (flonum=? X Y)
  (< (magnitude (- X Y)) EPSILON))


;;;; real and complex vector helpers

(define* (memory-real-vector->scheme-vector {nslots words.signed-int?} {P pointer?})
  (receive-and-return (V)
      (make-vector nslots #f)
    (do ((i 0 (fxadd1 i)))
	((fx=? i nslots))
      (vector-set! V i (array-ref-c-double P i)))))

(define* (memory-cplx-vector->scheme-vector {nslots words.signed-int?} {P pointer?})
  (receive-and-return (V)
      (make-vector nslots #f)
    (do ((i 0 (fxadd1 i))
	 (j 0 (+ 2 j)))
	((fx=? i nslots))
      (let ((rep (array-ref-c-double P j))
	    (imp (array-ref-c-double P (fxadd1 j))))
	(vector-set! V i (make-rectangular rep imp))))))

;;; --------------------------------------------------------------------

(define* (scheme-vector->memory-real-vector {V vector?})
  (define nslots (vector-length V))
  (define nbytes (* nslots sizeof-double))
  (receive-and-return (P)
      (guarded-malloc nbytes)
    (do ((i 0 (fxadd1 i)))
	((fx=? i nslots))
      (array-set-c-double! P i (vector-ref V i)))))

(define* (scheme-vector->memory-cplx-vector {V vector?})
  (define nslots (vector-length V))
  (define nbytes (* nslots sizeof-double-complex))
  (receive-and-return (P)
      (guarded-malloc nbytes)
    (do ((i 0 (fxadd1 i))
	 (j 0 (+ 2 j)))
	((fx=? i nslots))
      (let* ((Z   (vector-ref V i))
	     (rep (real-part Z))
	     (imp (imag-part Z)))
	(array-set-c-double! P j          rep)
	(array-set-c-double! P (fxadd1 j) imp)))))


;;;; real and complex matrix helpers

(define* (memory-real-matrix->scheme-vector {nrows words.signed-int?} {ncols words.signed-int?}
					    {P pointer?})
  (define nslots (* nrows ncols))
  (receive-and-return (V)
      (make-vector nslots #f)
    (do ((i 0 (fxadd1 i)))
	((fx=? i nslots))
      (vector-set! V i (array-ref-c-double P i)))))

(define* (memory-cplx-matrix->scheme-vector {nrows words.signed-int?} {ncols words.signed-int?}
					    {P pointer?})
  (define nslots (* nrows ncols))
  (receive-and-return (V)
      (make-vector nslots #f)
    (do ((i 0 (fxadd1 i))
	 (j 0 (+ 2 j)))
	((fx=? i nslots))
      (let ((rep (array-ref-c-double P j))
	    (imp (array-ref-c-double P (fxadd1 j))))
	(vector-set! V i (make-rectangular rep imp))))))

;;; --------------------------------------------------------------------

(define* (scheme-vector->memory-real-matrix {nrows words.signed-int?} {ncols words.signed-int?}
					    {V vector?})
  (define nslots (vector-length V))
  (define nbytes (* nslots sizeof-double))
  (assert (= nslots (* nrows ncols)))
  (receive-and-return (P)
      (guarded-malloc nbytes)
    (do ((i 0 (fxadd1 i)))
	((fx=? i nslots))
      (array-set-c-double! P i (vector-ref V i)))))

(define* (scheme-vector->memory-cplx-matrix {nrows words.signed-int?} {ncols words.signed-int?}
					    {V vector?})
  (define nslots (vector-length V))
  (define nbytes (* nslots sizeof-double-complex))
  (assert (= nslots (* nrows ncols)))
  (receive-and-return (P)
      (guarded-malloc nbytes)
    (do ((i 0 (fxadd1 i))
	 (j 0 (+ 2 j)))
	((fx=? i nslots))
      (let* ((Z   (vector-ref V i))
	     (rep (real-part Z))
	     (imp (imag-part Z)))
	(array-set-c-double! P j          rep)
	(array-set-c-double! P (fxadd1 j) imp)))))


(parametrise ((check-test-name	'version))

  (check
      (cephes_version_interface_current)
    => 1)

  (check
      (cephes_version_interface_age)
    => 0)

  (collect))


(parametrise ((check-test-name	'misc))

  (check
      (beta 1.2 3.4)
    (=> flonum=?)
    -0.035952833387741426)

  (collect))


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; End:
