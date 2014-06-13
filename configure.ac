dnl @configure_input@
dnl

AC_PREREQ([2.69])
AC_INIT([ATCephes],[0.1d0],
  [marco.maggi-ipsu@poste.it],
  [atcephes],[http://github.com/marcomaggi/atcephes/])
AC_CONFIG_SRCDIR([src/])
AC_CONFIG_MACRO_DIR([meta/autotools])
AC_CONFIG_AUX_DIR([meta/autotools])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([foreign dist-xz no-dist-gzip subdir-objects])
AM_MAINTAINER_MODE

AM_PROG_AR
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P

LT_PREREQ([2.4])
LT_INIT

dnl page
#### libraries interface version

dnl This is the version stored in the pkg-config data file.
AC_SUBST([CEPHES_PKG_CONFIG_VERSION],[0.1])

MM_LIBTOOL_LIBRARY_VERSIONS([cephes],1,0,0)

dnl page
#### basic system inspection

AC_LANG([C])
dnl AC_DEFINE([_GNU_SOURCE],[1],[enable everything GNU])
dnl AC_PROG_CC_C99
AM_PROG_AS
AC_PROG_CC
AM_PROG_CC_C_O
AC_HEADER_STDC
dnl AC_HEADER_SYS_WAIT
dnl AC_CHECK_HEADERS([assert.h stdint.h strings.h errno.h stddef.h sys/mman.h sys/resource.h unistd.h])
dnl AC_CHECK_SIZEOF(void *)
dnl AC_C_CONST
dnl AC_C_INLINE
dnl AC_C_RESTRICT
dnl AC_TYPE_LONG_LONG_INT
dnl AC_TYPE_UNSIGNED_LONG_LONG_INT
dnl AC_TYPE_SIZE_T
dnl AC_TYPE_SSIZE_T
dnl AC_TYPE_INT8_T
dnl AC_TYPE_INT16_T
dnl AC_TYPE_INT32_T
dnl AC_TYPE_INT64_T
dnl AC_TYPE_UINT8_T
dnl AC_TYPE_UINT16_T
dnl AC_TYPE_UINT32_T
dnl AC_TYPE_UINT64_T

AC_CACHE_SAVE

dnl page
#### checks for library functions

AC_FUNC_MALLOC
AC_FUNC_MEMCMP
dnl AC_CHECK_FUNCS([memmove memset strerror])
dnl AC_CHECK_FUNCS([mmap munmap msync mremap madvise mlock munlock mlockall munlockall mprotect])
dnl AC_CHECK_FUNCS([getenv])

AC_CACHE_SAVE

dnl page
#### external libraries

AC_CHECK_LIB([m],[cabs],
  [AS_VAR_SET([MATH_LIBS],[-lm])
   AS_VAR_SET([atcephes_FOUND_MATH_LIB],[yes])],
  [AS_VAR_SET([MATH_LIBS])
   AS_VAR_SET([atcephes_FOUND_MATH_LIB],[no])])
AC_SUBST([MATH_LIBS])
AM_CONDITIONAL([FOUND_MATH_LIB],[test "x$atcephes_FOUND_MATH_LIB" = xyes])

dnl page
#### finish

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile]
   [meta/scripts/libcephes.pc])
AC_OUTPUT

### end of file
# Local Variables:
# mode: autoconf
# page-delimiter: "^dnl page"
# End:
