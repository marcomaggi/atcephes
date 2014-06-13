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
 ** Type definitions.
 ** ----------------------------------------------------------------- */

/* Complex numeral.  */
typedef struct
{
  double r;
  double i;
} cmplx;


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

cephes_decl void   eclear (unsigned short * x);
cephes_decl void   emov   (unsigned short * a, unsigned short * b);
cephes_decl void   eabs   (unsigned short x[]);
cephes_decl void   eneg   (unsigned short x[]);
cephes_decl int    eisneg (unsigned short x[]);
cephes_decl int    eisinf (unsigned short x[]);
cephes_decl int    eisnan (unsigned short x[]);
cephes_decl void   einfin (unsigned short *x);
cephes_decl void   emovi  (unsigned short * a, unsigned short * b);
cephes_decl void   emovo  (unsigned short * a, unsigned short * b);
cephes_decl void   ecleaz (unsigned short * xi);
cephes_decl void   ecleazs (unsigned short *xi);
cephes_decl void   emovz  (unsigned short * a, unsigned short * b);
cephes_decl int    eiisnan (unsigned short x[]);
cephes_decl int    ecmpm  (unsigned short * a, unsigned short * b);
cephes_decl void   eshdn1 (unsigned short * x);
cephes_decl void   eshup1 (unsigned short * x);
cephes_decl void   eshdn8 (unsigned short * x);
cephes_decl void   eshup8 (unsigned short * x);
cephes_decl void   eshup6 (unsigned short * x);
cephes_decl void   eshdn6 (unsigned short * x);
cephes_decl void   eaddm  (unsigned short * x, unsigned short * y);
cephes_decl void   esubm  (unsigned short * x, unsigned short * y);
cephes_decl int    edivm  (unsigned short den[], unsigned short num[]);
cephes_decl int    emulm  (unsigned short a[], unsigned short b[]);
cephes_decl void   emdnorm (unsigned short s, int lost, int subflg,
			    long exp, int rcntrl);
cephes_decl void   esub   (unsigned short * a, unsigned short * b,
			   unsigned short * c);
cephes_decl void   eadd   (unsigned short * a, unsigned short * b,
			   unsigned short * c);
cephes_decl void   eadd1  (unsigned short * a, unsigned short * b,
			   unsigned short * c);
cephes_decl void   ediv   (unsigned short * a, unsigned short * b,
			   unsigned short * c);
cephes_decl void   emul   (unsigned short * a, unsigned short * b,
			   unsigned short * c);
cephes_decl void   e53toe (unsigned short * pe, unsigned short * y);
cephes_decl void   e64toe (unsigned short * pe, unsigned short * y);
cephes_decl void   e113toe (unsigned short * pe, unsigned short * y);
cephes_decl void   e24toe (unsigned short * pe, unsigned short * y);
cephes_decl void   etoe113 (unsigned short * x, unsigned short * e);
cephes_decl void   etoe64 (unsigned short * x, unsigned short * e);
cephes_decl void   etoe53 (unsigned short * x, unsigned short * e);
cephes_decl void   etoe24 (unsigned short * x, unsigned short * e);
cephes_decl int    ecmp   (unsigned short * a, unsigned short * b);
cephes_decl void   eround (unsigned short * x, unsigned short * y);
cephes_decl void   ltoe   (long * lp, unsigned short * y);
cephes_decl void   ultoe  (unsigned long *lp, unsigned short *y);
cephes_decl void   eifrac (unsigned short *x, long *i, unsigned short *frac);
cephes_decl void   euifrac (unsigned short *x, unsigned long *i, unsigned short *frac);
cephes_decl int    eshift (unsigned short *x, int sc);
cephes_decl int    enormlz (unsigned short x[]);
cephes_decl void   e24toasc (unsigned short x[], char *string, int ndigs);
cephes_decl void   e53toasc (unsigned short x[], char *string, int ndigs);
cephes_decl void   e64toasc (unsigned short x[], char *string, int ndigs);
cephes_decl void   e113toasc (unsigned short x[], char *string, int ndigs);
cephes_decl void   etoasc (unsigned short x[], char *string, int ndigs);
cephes_decl void   asctoe24 (char *s, unsigned short *y);
cephes_decl void   asctoe53 (char *s, unsigned short *y);
cephes_decl void   asctoe64 (char *s, unsigned short *y);
cephes_decl void   asctoe113 (char *s, unsigned short *y);
cephes_decl void   asctoe (char *s, unsigned short *y);
cephes_decl void   asctoeg (char *ss, unsigned short *y, int oprec);
cephes_decl void   efloor (unsigned short x[], unsigned short y[]);
cephes_decl void   efrexp (unsigned short x[], long *exp, unsigned short s[]);
cephes_decl void   eldexp (unsigned short x[], long pwr2, unsigned short y[]);
cephes_decl void   eremain (unsigned short a[], unsigned short b[],
			    unsigned short c[]);
cephes_decl void   eiremain (unsigned short den[], unsigned short num[]);
cephes_decl void   enan (unsigned short *nan, int size);
cephes_decl void   esqrt (short *x, short *y);

cephes_decl void   eexp   (unsigned short * x, unsigned short * y);
cephes_decl void   epow   (unsigned short * x, unsigned short * y,
			   unsigned short * z);
cephes_decl void   epowi  (unsigned short x[], unsigned short y[],
			   unsigned short z[]);
cephes_decl void   etodec (unsigned short * x, unsigned short * d);
cephes_decl void   dectoe (unsigned short * d, unsigned short * e);

/* ------------------------------------------------------------------ */

cephes_decl double beta (double a, double b);
cephes_decl double dawsn (double xx);
cephes_decl double ei (double x);
cephes_decl double expn (int n, double x);
cephes_decl double fac (int i);
cephes_decl int    fresnl (double xxa, double * ssa, double * cca);
cephes_decl double plancki (double w, double T);
cephes_decl double polylog (int n, double x);
cephes_decl void   revers (double y[], double x[], int n);
cephes_decl double rgamma (double x);
cephes_decl int    shichi (double x, double * si, double * ci);
cephes_decl int    sici (double x, double *si, double *ci);
cephes_decl double simpsn (double f[], double delta);
cephes_decl double spence (double x);
cephes_decl double zeta (double x, double q);
cephes_decl double zetac (double x);

/* ------------------------------------------------------------------ */

cephes_decl void   polini (int maxdeg);
cephes_decl void   polprt (double a[], int na, int d);
cephes_decl void   polclr (double *a, int n);
cephes_decl void   polmov (double *a, double *b, int na);
cephes_decl void   polmul (double a[], double b[], double c[],
			   int na, int nb);
cephes_decl void   poladd (double a[], double b[], double c[],
			   int na, int nb);
cephes_decl void   polsub (double a[], double b[], double c[],
			   int na, int nb);
cephes_decl int    poldiv (double a[], double b[], double c[],
			   int na, int nb);
cephes_decl void   polsbt (double a[], double b[], double c[],
			   int na, int nb);
cephes_decl double poleva (double a[], int na, double x);

cephes_decl void   polatn (double num[], double den[], double ans[],
			   int nn);
cephes_decl void   polsqt (double pol[], double ans[], int nn);
cephes_decl void   polsin (double x[], double y[], int nn);
cephes_decl void   polcos (double x[], double y[], int nn);
cephes_decl int    polrt  (double xcof[], double cof[], int m, cmplx root[]);


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* CEPHES_H */

/* end of file */
