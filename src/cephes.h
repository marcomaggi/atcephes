/*
  Part of: ATCephes
  Contents: public header file
  Date: Fri Jun 13, 2014

  Abstract



  Copyright 2014 by Stephen L. Moshier
  This head file was Written by Marco Maggi <marco.maggi-ipsu@poste.it>

  The author hereby  grant permission to use,  copy, modify, distribute,
  and  license this  software  and its  documentation  for any  purpose,
  provided that  existing copyright notices  are retained in  all copies
  and that  this notice  is included verbatim  in any  distributions. No
  written agreement, license, or royalty fee  is required for any of the
  authorized uses.  Modifications to this software may be copyrighted by
  their authors and need not  follow the licensing terms described here,
  provided that the new terms are clearly indicated on the first page of
  each file where they apply.

  IN NO  EVENT SHALL THE AUTHOR  OR DISTRIBUTORS BE LIABLE  TO ANY PARTY
  FOR  DIRECT, INDIRECT, SPECIAL,  INCIDENTAL, OR  CONSEQUENTIAL DAMAGES
  ARISING OUT  OF THE  USE OF THIS  SOFTWARE, ITS DOCUMENTATION,  OR ANY
  DERIVATIVES  THEREOF, EVEN  IF THE  AUTHOR  HAVE BEEN  ADVISED OF  THE
  POSSIBILITY OF SUCH DAMAGE.

  THE  AUTHOR  AND DISTRIBUTORS  SPECIFICALLY  DISCLAIM ANY  WARRANTIES,
  INCLUDING,   BUT   NOT  LIMITED   TO,   THE   IMPLIED  WARRANTIES   OF
  MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE,    AND
  NON-INFRINGEMENT.  THIS  SOFTWARE IS PROVIDED  ON AN "AS  IS" BASIS,
  AND  THE  AUTHOR  AND  DISTRIBUTORS  HAVE  NO  OBLIGATION  TO  PROVIDE
  MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
*/

#ifndef CEPHES_H
#define CEPHES_H 1


/** --------------------------------------------------------------------
 ** Common preprocessor macros.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
extern "C" {
#endif

/* The macro CEPHES_UNUSED indicates  that a function, function argument
   or variable may potentially be unused. Usage examples:

   static int unused_function (char arg) CEPHES_UNUSED;
   int foo (char unused_argument CEPHES_UNUSED);
   int unused_variable CEPHES_UNUSED;
*/
#ifdef __GNUC__
#  define CEPHES_UNUSED		__attribute__((unused))
#else
#  define CEPHES_UNUSED		/* empty */
#endif

#ifndef __GNUC__
#  define __attribute__(...)	/* empty */
#endif

/* I found  the following chunk on  the Net.  (Marco Maggi;  Sun Feb 26,
   2012) */
#if defined _WIN32 || defined __CYGWIN__
#  ifdef BUILDING_DLL
#    ifdef __GNUC__
#      define cephes_decl		__attribute__((dllexport))
#    else
#      define cephes_decl		__declspec(dllexport)
#    endif
#  else
#    ifdef __GNUC__
#      define cephes_decl		__attribute__((dllimport))
#    else
#      define cephes_decl		__declspec(dllimport)
#    endif
#  endif
#  define cephes_private_decl	extern
#else
#  if __GNUC__ >= 4
#    define cephes_decl		__attribute__((visibility ("default")))
#    define cephes_private_decl	__attribute__((visibility ("hidden")))
#  else
#    define cephes_decl		extern
#    define cephes_private_decl	extern
#  endif
#endif


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include <math.h>


/** --------------------------------------------------------------------
 ** Function prototypes.
 ** ----------------------------------------------------------------- */

cephes_decl double chbevl (double x, double array[], double n);
cephes_decl int    drand  (double * a);
cephes_decl double polevl (double x, double coef[], int N);
cephes_decl double p1evl  (double x, double coef[], int N);
cephes_decl double powi   (double x, int nn);
cephes_decl double sindg  (double x);
cephes_decl double tandg  (double x);
cephes_decl double cotdg  (double x);

/* ------------------------------------------------------------------ */

cephes_decl int    airy   (double x, double * ai, double * aip,
			   double * bi, double * bip);
cephes_decl double hyp2f1 (double a, double b, double c, double x);
cephes_decl double hyperg (double a, double b, double x);
cephes_decl double i0     (double x);
cephes_decl double i0e    (double x);
cephes_decl double i1     (double x);
cephes_decl double i1e    (double x);
cephes_decl double iv     (double v, double x);
cephes_decl double jv     (double n, double x);
cephes_decl double k0     (double x);
cephes_decl double k0e    (double x);
cephes_decl double k1     (double x);
cephes_decl double k1e    (double x);
cephes_decl double kn     (int nn, double x);
cephes_decl double psi    (double x);
cephes_decl double struve (double v, double x);

/* ------------------------------------------------------------------ */

cephes_decl double bdtr   (int k, int n, double p);
cephes_decl double bdtrc  (int k, int n, double p);
cephes_decl double bdtri  (int k, int n, double y);
cephes_decl double btdtr  (double a, double b, double x);
cephes_decl double chdtrc (double df, double x);
cephes_decl double chdtr  (double df, double x);
cephes_decl double chdtri (double df, double y);
cephes_decl double expx2  (double x, int sign);
cephes_decl double fdtr   (int ia, int ib, double x);
cephes_decl double fdtrc  (int ia, int ib, double x);
cephes_decl double fdtri  (int ia, int ib, double y);
cephes_decl double gamma  (double x);
cephes_decl double lgam   (double x);
cephes_decl double gdtr   (double a, double b, double x);
cephes_decl double gdtrc  (double a, double b, double x);
cephes_decl double igam   (double a, double x);
cephes_decl double igamc  (double a, double x);
cephes_decl double igami  (double a, double y0);
cephes_decl double incbet (double aa, double bb, double xx);
cephes_decl double incbi  (double aa, double bb, double yy0);
cephes_decl double kolmogorov (double y);
cephes_decl double nbdtr  (int k, int n, double p);
cephes_decl double nbdtrc (int k, int n, double p);
cephes_decl double nbdtri (int k, int n, double p);
cephes_decl double ndtr   (double a);
cephes_decl double erfc   (double a);
cephes_decl double erf    (double x);
cephes_decl double ndtri  (double y0);
cephes_decl double pdtr   (int k, double m);
cephes_decl double pdtrc  (int k, double m);
cephes_decl double pdtri  (int k, double y);
cephes_decl double stdtr  (int k, double t);
cephes_decl double stdtri (int k, double p);

/* ------------------------------------------------------------------ */

cephes_decl double ellie  (double phi, double m);
cephes_decl double ellik  (double phi, double m);
cephes_decl double ellpe  (double x);
cephes_decl int    ellpj  (double u, double m,
			   double *sn, double *cn, double *dn, double *ph);
cephes_decl double ellpk  (double x);

/* ------------------------------------------------------------------ */




/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* CEPHES_H */

/* end of file */
