dnl -*- Autoconf -*-

AC_INIT([guix-data-service], [m4_translit(m4_esyscmd([cat VERSION]),m4_newline)])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([gnu color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

GUILE_PKG([2.2])
GUILE_PROGS
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.2 installation.])
fi

GUILE_MODULE_AVAILABLE([have_json], [(json)])
if test "x$have_json" != "xyes"; then
  AC_MSG_ERROR([Guile-JSON is missing; please install it.])
fi

GUILE_MODULE_AVAILABLE([have_guile_squee], [(squee)])
if test "x$have_guile_squee" != "xyes"; then
  AC_MSG_ERROR([Guile-Squee is missing; please install it.])
fi

GUILE_MODULE_AVAILABLE([have_fibers], [(fibers web server)])
if test "x$have_fibers" != "xyes"; then
  AC_MSG_ERROR([Guile fibers is missing; please install it.])
fi

GUILE_MODULE_AVAILABLE([have_email], [(email email)])
if test "x$have_email" != "xyes"; then
  AC_MSG_ERROR([Guile email is missing; please install it.])
fi

guilemoduledir="${datarootdir}/guile/site/${GUILE_EFFECTIVE_VERSION}"
AC_SUBST([guilemoduledir])
AC_SUBST([GUILE_EFFECTIVE_VERSION])

dnl Substitute placeholders to generate these target files
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([guix-data-service/config.scm])
AC_CONFIG_FILES([scripts/guix-data-service], [chmod +x scripts/guix-data-service])
AC_CONFIG_FILES([scripts/guix-data-service-process-jobs], [chmod +x scripts/guix-data-service-process-jobs])
AC_CONFIG_FILES([scripts/guix-data-service-process-branch-updated-email], [chmod +x scripts/guix-data-service-process-branch-updated-email])
AC_CONFIG_FILES([scripts/guix-data-service-query-build-servers], [chmod +x scripts/guix-data-service-query-build-servers])
AC_CONFIG_FILES([test-env:build-aux/test-env.in], [chmod +x test-env])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT
