## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS		= -I meta/autotools
AUTOMAKE_OPTIONS	= foreign
EXTRA_DIST		= INSTALL configure.sh
dist_doc_DATA		= README COPYING
AM_CFLAGS		= -Wall -I$(srcdir)/src/cmath

## --------------------------------------------------------------------

EXTRA_DIST		+= meta/scripts/libcephes.pc.in
pkgconfigdir		= $(libdir)/pkgconfig
nodist_pkgconfig_DATA	= meta/scripts/libcephes.pc

## --------------------------------------------------------------------

# AM_MAKEINFOFLAGS	= --no-split

# info_TEXINFOS		= doc/cephes.texi
# doc_cephes_TEXINFOS	= \
# 	doc/macros.texi		\
# 	doc/gpl-3.0.texi	\
# 	doc/fdl-1.3.texi

#page
#### libraries

cephes_CURRENT		= @cephes_VERSION_INTERFACE_CURRENT@
cephes_REVISION		= @cephes_VERSION_INTERFACE_REVISION@
cephes_AGE		= @cephes_VERSION_INTERFACE_AGE@

include_HEADERS		= src/cephes.h

lib_LTLIBRARIES		= libcephes.la
libcephes_la_LDFLAGS	= -version-info $(cephes_CURRENT):$(cephes_REVISION):$(cephes_AGE)
libcephes_la_LIBADD	= $(MATH_LIBS)
libcephes_la_SOURCES	= \
	src/custom/isfinite.c		\
	\
	src/cmath/chbevl.c		\
	src/cmath/const.c		\
	src/cmath/drand.c		\
	src/cmath/mconf.h		\
	src/cmath/mtherr.c		\
	src/cmath/polevl.asm		\
	src/cmath/polevl.c		\
	src/cmath/powi.c		\
	src/cmath/setprbor.asm		\
	src/cmath/setprec.c		\
	src/cmath/sindg.c		\
	src/cmath/tandg.c		\
	src/cmath/unity.c		\
	\
	src/bessel/airy.c		\
	src/bessel/hyp2f1.c		\
	src/bessel/hyperg.c		\
	src/bessel/i0.c			\
	src/bessel/i1.c			\
	src/bessel/iv.c			\
	src/bessel/jv.c			\
	src/bessel/k0.c			\
	src/bessel/k1.c			\
	src/bessel/kn.c			\
	src/bessel/psi.c		\
	src/bessel/struve.c		\
	\
	src/cprob/bdtr.c		\
	src/cprob/btdtr.c		\
	src/cprob/chdtr.c		\
	src/cprob/expx2.c		\
	src/cprob/fdtr.c		\
	src/cprob/gamma.c		\
	src/cprob/gdtr.c		\
	src/cprob/igam.c		\
	src/cprob/igami.c		\
	src/cprob/incbet.c		\
	src/cprob/incbi.c		\
	src/cprob/kolmogorov.c		\
	src/cprob/mconf.h		\
	src/cprob/nbdtr.c		\
	src/cprob/ndtr.c		\
	src/cprob/ndtri.c		\
	src/cprob/pdtr.c		\
	src/cprob/stdtr.c		\
	\
	src/ellf/ellie.c		\
	src/ellf/ellik.c		\
	src/ellf/ellpe.c		\
	src/ellf/ellpj.c		\
	src/ellf/ellpk.c		\
	src/ellf/mconf.h		\
	src/ellf/protos.h		\
	\
	src/ieee/econst.c		\
	src/ieee/eexp.c			\
	src/ieee/ehead.h		\
	src/ieee/elog.c			\
	src/ieee/epow.c			\
	src/ieee/etanh.c		\
	src/ieee/etodec.c		\
	src/ieee/ieee.c			\
	src/ieee/mconf.h		\
	\
	src/misc/beta.c			\
	src/misc/dawsn.c		\
	src/misc/ei.c			\
	src/misc/expn.c			\
	src/misc/fac.c			\
	src/misc/fresnl.c		\
	src/misc/mconf.h		\
	src/misc/planck.c		\
	src/misc/polylog.c		\
	src/misc/revers.c		\
	src/misc/rgamma.c		\
	src/misc/shichi.c		\
	src/misc/sici.c			\
	src/misc/simpsn.c		\
	src/misc/spence.c		\
	src/misc/zeta.c			\
	src/misc/zetac.c		\
	\
	src/polyn/euclid.c		\
	src/polyn/polmisc.c		\
	src/polyn/polrt.c		\
	src/polyn/polyn.c

## --------------------------------------------------------------------

EXTRA_DIST	+= \
	src/cmath/protos.h

# The following are already included in the C99 Standard.
EXTRA_DIST	+= \
	src/cmath/acosh.c		\
	src/cmath/asin.c		\
	src/cmath/asinh.c		\
	src/cmath/atan.c		\
	src/cmath/atanh.c		\
	src/cmath/cbrt.c		\
	src/cmath/cmplx.c		\
	src/cmath/clog.c		\
	src/cmath/cosh.c		\
	src/cmath/exp.c			\
	src/cmath/exp10.c		\
	src/cmath/exp2.c		\
	src/cmath/fabs.c		\
	src/cmath/floor.c		\
	src/cmath/isnan.c		\
	src/cmath/log.c			\
	src/cmath/log10.c		\
	src/cmath/log2.c		\
	src/cmath/pow.c			\
	src/cmath/round.c		\
	src/cmath/sin.c			\
	src/cmath/sincos.c		\
	src/cmath/sinh.c		\
	src/cmath/sqrt.c		\
	src/cmath/tan.c			\
	src/cmath/tanh.c		\
	src/bessel/yn.c

# The following are already included in the C99 Standard.
EXTRA_DIST	+= \
	src/bessel/j0.c			\
	src/bessel/j1.c			\
	src/bessel/jn.c

# The following are test programs.
EXTRA_DIST	+= \
	src/cmath/dtestvec.c		\
	src/cmath/mod2pi.c		\
	src/cmath/mtst.c		\
	src/ellf/ellf.c			\
	src/ieee/ieetst.c		\
	src/remes/remes.c

# The following is included in cmath already.
EXTRA_DIST	+= \
	src/cprob/const.c		\
	src/cprob/drand.c		\
	src/cprob/mtherr.c		\
	src/cprob/polevl.c		\
	src/cprob/unity.c

# The following is included in cmath already.
EXTRA_DIST	+= \
	src/ellf/cmplx.c		\
	src/ellf/const.c		\
	src/ellf/mtherr.c		\
	src/ellf/polevl.c

# The following is included in cmath already.
EXTRA_DIST	+= \
	src/ieee/drand.c		\
	src/ieee/mtherr.c

# The following is included in cmath already.
EXTRA_DIST	+= \
	src/misc/chbevl.c		\
	src/misc/mtherr.c		\
	src/misc/polevl.c		\
	src/misc/psi.c

# The following is included in misc already.
EXTRA_DIST	+= \
	src/polyn/revers.c

# Conflicting names with other source file.
EXTRA_DIST	+= \
	src/polyn/polyr.c

# Is there some interest in including these?
EXTRA_DIST	+= \
	src/ode/adams3.c		\
	src/ode/int.h			\
	src/ode/rungek.c		\
	src/ode/ssystem.c

# What should I do about these?
EXTRA_DIST	+= \
	src/remes/remes.h		\
	src/remes/remesa.c		\
	src/remes/remese.c		\
	src/remes/remesf.c		\
	src/remes/remesg.c		\
	src/remes/remesp.c		\
	src/remes/remess.c		\
	src/remes/remesw.c		\
	src/remes/simq.c

#page
#### tests

check_PROGRAMS	= \
	tests/beta

TESTS				= $(check_PROGRAMS)

cephes_tests_cppflags		= -I$(srcdir)/src
cephes_tests_ldadd		= libcephes.la

tests_beta_CPPFLAGS		= $(cephes_tests_cppflags)
tests_beta_LDADD		= $(cephes_tests_ldadd)

installcheck-local: $(TESTS)
	@for f in $(TESTS); do $(builddir)/$$f; done

### end of file
