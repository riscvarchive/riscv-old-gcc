AC_INIT(risc-gcc, 1.0)

AC_PROG_CC
AC_PROG_AWK

AC_CHECK_LIB(gmp, __gmpz_mul_si, ,
       [AC_MSG_ERROR(
       [GNU MP not found, or not 3.1 or up, see http://gmplib.org/])])

AC_CHECK_LIB(mpfr, mpfr_add, , [AC_MSG_ERROR(
     [Need MPFR either from GNU MP 4 or separate MPFR package.
     See http://www.mpfr.org or http://swox.com/gmp])])

AC_TRY_COMPILE([#include <mpc.h>],[
    #if MPC_VERSION < MPC_VERSION_NUM(0,8,0)
    choke me
    #endif
], [AC_TRY_COMPILE([#include <mpc.h>],[
    #if MPC_VERSION < MPC_VERSION_NUM(0,8,1)
    choke me
    #endif
], [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([buggy but acceptable])])],
     [AC_MSG_RESULT([no]); have_gmp=no])

AC_OUTPUT(
    Makefile
)
