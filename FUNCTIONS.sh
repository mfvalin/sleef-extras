#!/bin/bash
# Copyright Recherche en Prevision Numerique and contributors 2020.
# Distributed under the Boost Software License, Version 1.0.
# https://www.boost.org/LICENSE_1_0.txt
# wrappers for libsleef https://sleef.org
#
# M.Valin 2020/07/31
#
set -x
#prefixes for the various interfaces
export Vlibm=${Vlibm:-Vm_}            # functions calling libm
export Vlibsleef=${Vlibsleef:-Vsl_}   # functions calling libsleef
export Vfortran=${Vfortran:-V_}       # Fortran interface to functions calling libsleef
#
# the above environment variables are used by scripts FUNCTION.sh, FUNCTION_1_2.sh, FUNCTION_2_1.sh
#
# generate C prototypes for functions with some possibly missing prototypes for libm funcitons
cat <<EOT  >functions.h
//!   Copyright Recherche en Prevision Numerique and contributors 2020.
//!   Distributed under the Boost Software License, Version 1.0.
//!   https://www.boost.org/LICENSE_1_0.txt
//!   wrappers for libsleef https://sleef.org
//!   M.Valin 2020/07/31
#if ! defined(_GNU_SOURCE)
  void sincos(double x, double *sin, double *cos);
  void sincosf(float x, float *sin, float *cos);
  double exp10(double x);
  float exp10f(float x);
#endif
EOT
#
# ULP evaluation C functions and their Fortran interface (float and double versions)
#
# timing function and its Fortran interface (returns clock cycles)
# for architectures other than X86-64(__x86_64__) and ARM-64(__aarch64__), 
# a dummy timing function is generated
cat <<EOT >>functions.h
//===========================================================================================
// interface ${Vfortran}bit_diff
//   subroutine Vsl_bit_diff_f(f1, f2, n, lo, hi, avg) BIND(C,name='Vsl_bit_diff_f')
//     import :: C_FLOAT, C_INT
//     real(C_FLOAT), dimension(*), intent(IN) :: f1, f2
//     integer(C_INT), intent(IN), value :: n
//     integer(C_INT), intent(OUT) :: lo, hi
//     real(C_FLOAT), intent(OUT) :: avg
//   end subroutine Vsl_bit_diff_f
//   subroutine Vsl_bit_diff_d(f1, f2, n, lo, hi, avg) BIND(C,name='Vsl_bit_diff_d')
//     import :: C_FLOAT, C_INT, C_DOUBLE
//     real(C_DOUBLE), dimension(*), intent(IN) :: f1, f2
//     integer(C_INT), intent(IN), value :: n
//     integer(C_INT), intent(OUT) :: lo, hi
//     real(C_FLOAT), intent(OUT) :: avg
//   end subroutine Vsl_bit_diff_d
// end interface ${Vfortran}bit_diff
//===========================================================================================
void Vsl_bit_diff_f(float *f1, float *f2, int n, int *minlsbs, int *maxlsbs, float *avglsbs){
  uint32_t *if1 = (uint32_t *) f1 ;  // IEEE floats treated as unsigned ints for comparison purposes
  uint32_t *if2 = (uint32_t *) f2 ;  // leftmost bit will be suppressed to get absolute value
  int i, mini, maxi, toti, nlsb ;
  mini = 0x7FFFFFFF ; maxi = 0 ; toti = 0 ;
  for(i = 0 ; i < n ; i++){
    // if( ((if1[i] >> 23) & 0xFF) == 0xFF ) continue ;         // ignore if f1 == nan
    nlsb = (if1[i] & 0x7FFFFFFF) - (if2[i] & 0x7FFFFFFF) ;   // difference between absolute values
    nlsb = (nlsb < 0) ? -nlsb : nlsb ;                       // absolute value of difference
    mini = (nlsb < mini) ? nlsb : mini ;                     // minimum
    maxi = (nlsb > maxi) ? nlsb : maxi ;                     // maximum
    toti += nlsb ;                                           // sum
  }
  *avglsbs = toti ;
  *avglsbs /= n ;
  *maxlsbs = maxi ;
  *minlsbs = mini ;
}
//===========================================================================================
void Vsl_bit_diff_d(double *f1, double *f2, int n, int *minlsbs, int *maxlsbs, float *avglsbs){
  uint64_t *if1 = (uint64_t *) f1 ;
  uint64_t *if2 = (uint64_t *) f2 ;
  int64_t i, mini, maxi, toti, nlsb ;
  mini = 0x7FFFFFFF ; maxi = 0 ; toti = 0 ;
  for(i = 0 ; i < n ; i++){
    // if( ((if1[i] >> 52) & 0x7FF) == 0x7FF ) continue ;  // ignore if f1 == nan
    nlsb = (if1[i] & 0x7FFFFFFFFFFFFFFFL) - (if2[i] & 0x7FFFFFFFFFFFFFFFL) ;
    nlsb = (nlsb > 0) ? nlsb : -nlsb ;
    mini = (nlsb < mini) ? nlsb : mini ;
    maxi = (nlsb > maxi) ? nlsb : maxi ;
    toti += nlsb ;
  }
  *avglsbs = toti ;
  *avglsbs /= n ;
  *maxlsbs = maxi ;
  *minlsbs = mini ;
}
//===========================================================================================
// interface 
//   function ${Vfortran}rdtsc() result(t) BIND(C,name='Vsl_rdtsc')
//     import :: C_LONG_LONG
//     integer(C_LONG_LONG) :: t
//   end function ${Vfortran}rdtsc
// end interface
static uint64_t time0 = 0;
uint64_t ${Vlibsleef}rdtsc(void) {   // serialized version on X86
#if defined(__x86_64__) || defined(USE_RDTSCP)
  uint32_t lo, hi, rcx;
  __asm__ volatile ("rdtscp" :  "=a" (lo), "=d" (hi), "=c" (rcx) );
  return (uint64_t)lo | (((uint64_t)hi) << 32);
#endif
#if defined(__aarch64__)
  asm volatile ("isb; mrs %0, cntvct_el0" : "=r" (time0));
  return time0;
#endif
#if !defined(__x86_64__) && !defined(__aarch64__)
  return time0++;  // dummy for the time being
#endif
}

EOT
# generate C code for functions with one input, one output
./FUNCTION.sh sin10 sin35 cos10 cos35 tan10 tan35 \
              log10 log35 log210 log235 log1p10 log1010 exp10 exp210 exp1010 expm110 \
              sqrt05 sqrt35 cbrt10 cbrt35 \
              asin10 asin35 acos10 acos35 atan10 atan35 \
              sinh10 sinh35 cosh10 cosh35 tanh10 tanh35 \
              asinh10 acosh10 atanh10 \
              erf10 erfc15 tgamma10 lgamma10                 >>functions.h
# generate C code for functions with two inputs, one output
./FUNCTION_1_2.sh atan210 atan235 pow10 hypot05 hypot35      >>functions.h
# generate C code for functions with one input, twu outputs
./FUNCTION_2_1.sh sincos10 sincos35                          >>functions.h
#
# now, the Fortran include file
grep '^//[ !]' functions.h | sed 's:^//::' >sleef.inc
