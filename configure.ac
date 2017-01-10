#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.53)
AC_PACKAGE_VERSION(1.0.8)
AC_INIT(fast_yaml, 1.0.8, [], [])

# Checks for programs.
AC_PROG_CC
AC_PROG_MAKE_SET

if test "x$GCC" = "xyes"; then
    CFLAGS="$CFLAGS -Wall"
fi

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Checks for library functions.
AC_FUNC_MALLOC
AC_HEADER_STDC

# Checks Erlang runtime and compiler
AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC

# Checks and sets ERLANG_ROOT_DIR and ERLANG_LIB_DIR variable
# AC_ERLANG_SUBST_ROOT_DIR
# AC_ERLANG_SUBST_LIB_DIR

AC_CHECK_HEADERS([yaml.h], [], [
    AC_MSG_ERROR([libyaml header file yaml.h was not found])])

AC_SEARCH_LIBS([yaml_parser_initialize], [yaml], [], [
    AC_MSG_ERROR([libyaml library was not found])])

AC_ARG_ENABLE(gcov,
[AC_HELP_STRING([--enable-gcov], [compile with gcov enabled (default: no)])],
[case "${enableval}" in
  yes) gcov=true ;;
  no)  gcov=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-gcov) ;;
esac],[gcov=false])

AC_SUBST(gcov)

AC_CONFIG_FILES([vars.config])
AC_OUTPUT
