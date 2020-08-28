! !/bin/bash
!  Copyright Recherche en Prevision Numerique and contributors 2020.
!  Distributed under the Boost Software License, Version 1.0.
!  https://www.boost.org/LICENSE_1_0.txt
!  test for libsleef (https://sleef.org) wrappers
! 
!  M.Valin 2020/07/31
!
! timing and validation test for vector subroutines calling libsleef wrapper
program test_sleef
  use ISO_C_BINDING
  implicit none

! define whether libm or the Fortran library will be used as the reference for validation

! reference for cubic root will be libm if USE_LIBM_cbrt is defined
#define USE_LIBM_cbrt

! reference for sincos will be libm if USE_LIBM_sincos is defined
#define USE_LIBM_sincos__

! reference for pow (exponentiation) will be libm if USE_LIBM_pow is defined
#define USE_LIBM_pow__

! reference for all functions with prototypes in libm.hf will be libm if USE_LIBM_ALL is defined
#define USE_LIBM_ALL__

! Fortran interface to some libm functions
#include <libm.hf>

! Fortran prototypes generated with FUNCTIONS.sh
  include 'sleef.inc'

! number of points for timing test (may be defined when calling make)
#if !defined NPTS
#define NPTS 40
#endif

  real :: CLOCK
  integer :: NP
  real(C_FLOAT), dimension(NPTS) :: f4
  real(C_FLOAT), dimension(NPTS) :: r4, r4b, r4a, r4c, r4d
  real(C_DOUBLE), dimension(NPTS) :: f8
  real(C_DOUBLE), dimension(NPTS) :: r8, r8b, r8a, r8c, r8d
  integer(C_LONG_LONG) :: t0, t1, t2, t3, t4, t(4)
  integer :: i, j, mi4, ma4, mi8, ma8, larg, stat
  real(C_FLOAT) :: avg4, avg8
  character(len=128) :: argv(2)
  real(C_FLOAT) :: NP1

  call get_command_argument(1,argv(1),larg,stat)    ! get 1 program argument
  if(stat .ne. 0 .or. argv(1)(1:2) == '-h' .or. argv(1)(1:3) == '--h' ) then
    call get_command_argument(0,argv(1),larg,stat)
    print *,"usage : "//trim(argv(1))//" number_of_points"
    print *,"  ex.   "//trim(argv(1))//" 128"
    goto 777
  endif
  read(argv(1),*,err=777) NP                        ! number of points
  if(NP > NPTS) then
    print *,"timing tests NP =", NP
    print *,"NP too large, max allowed =", NPTS
    print *,"the program should be recompiled with a larger value for NPTS"
    print *,"make NPTS=...."
    goto 777
  endif
  call get_command_argument(2,argv(2),larg,stat)    ! get 1 program argument
  if(stat .ne. 0) then
    print *,"clock rate not specified, using 3.7 GHz"
    CLOCK = 3.7
  else
    read(argv(2),*,err=777) CLOCK
  endif
  print '(A,I0,A,F5.2,A)',"testing for ",NP," points, timer clock =",CLOCK,"GHz"

  NP1 = 1.0 / REAL(NP)
  do i = 1, NP
    f4(i) = (10.0  * i) * NP1
    f8(i) = 10.0_8 * i * NP1
  enddo

! V_rdtsc() reads the time stamp counter (X86 only at this moment)
  print 2,'  function    ftn-r4  sleef-r4    ftn-r8  sleef-r8 (ns/result) MAX4  MAX8  ULP4  ULP8'
2 format(A)
  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = sin(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_sin(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = sin(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_sin(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'sin',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = sin(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_sin35(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = sin(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_sin35(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'sin35',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = cos(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_cos(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = cos(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_cos(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'cos',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = cos(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_cos35(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = cos(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_cos35(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'cos35',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4a = 999999.0
    r8a = 999999.0
    r4b = 999999.0
    r8b = 999999.0
    r4c = 999999.0
    r8c = 999999.0
    r4d = 999999.0
    r8d = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
#if defined(USE_LIBM_sincos)
      call sincos(f4(i), r4c(i), r4d(i))
#else
      r4c(i)  = sin(f4(i))
      r4d(i)  = cos(f4(i))
#endif
    enddo
    t1 = V_rdtsc()
    call v_sincos(f4, r4a, r4b, NP)
    t2 = V_rdtsc()
    do i = 1, NP
#if defined(USE_LIBM_sincos)
      call sincos(f8(i), r8c(i), r8d(i))
#else
      r8c(i)  = sin(f8(i))
      r8d(i)  = cos(f8(i))
#endif
    enddo
    t3 = V_rdtsc()
    call v_sincos(f8, r8a, r8b, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
! print *,r4a(1:NP)
! print *,'----'
! print *,r4b(1:NP)
! print *,'----'
! print *,r4c(1:NP)
! print *,'----'
! print *,r4d(1:NP)
! print *,'----'
  call V_bit_diff(r4a, r4c, NP, mi4, ma4, avg4)
  call V_bit_diff(r8a, r8c, NP, mi8, ma8, avg8)
  print 1,'SINcos',t/CLOCK*NP1,ma4, ma8, avg4, avg8
  call V_bit_diff(r4b, r4d, NP, mi4, ma4, avg4)
  call V_bit_diff(r8b, r8d, NP, mi8, ma8, avg8)
  print 1,'sinCOS',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    r4a = 999999.0
    r8a = 999999.0
    r4c = 999999.0
    r8c = 999999.0
    r4d = 999999.0
    r8d = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
#if defined(USE_LIBM_sincos)
      call sincos(f4(i), r4c(i), r4d(i))
#else
      r4c(i) = sin(f4(i))
      r4d(i) = cos(f4(i))
#endif
    enddo
    t1 = V_rdtsc()
    call v_sincos35(f4, r4, r4a, NP)
    t2 = V_rdtsc()
    do i = 1, NP
#if defined(USE_LIBM_sincos)
      call sincos(f8(i), r8c(i), r8d(i))
#else
      r8c(i) = sin(f8(i))
      r8d(i) = cos(f8(i))
#endif
    enddo
    t3 = V_rdtsc()
    call v_sincos35(f8, r8, r8a, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4c, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8c, NP, mi8, ma8, avg8)
  print 1,'SINcos35',t/CLOCK*NP1,ma4, ma8, avg4, avg8
  call V_bit_diff(r4a, r4d, NP, mi4, ma4, avg4)
  call V_bit_diff(r8a, r8d, NP, mi8, ma8, avg8)
  print 1,'sinCOS35',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = tan(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_tan(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = tan(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_tan(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'tan',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = tan(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_tan35(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = tan(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_tan35(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'tan35',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  do i = 1, NP
    f4(i) = (1.0 - (2.0*i)*NP1) + .000001
    f8(i) = (1.0 - (2.0*i)*NP1) + .000001
  enddo

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = asin(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_asin(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = asin(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_asin(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'asin',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = asin(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_asin35(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = asin(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_asin35(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'asin35',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = acos(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_acos(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = acos(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_acos(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'acos',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = acos(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_acos35(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = acos(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_acos35(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'acos35',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = atan(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_atan(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = atan(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_atan(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'atan',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = atan(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_atan35(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = atan(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_atan35(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'atan35',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  do i = 1, NP
    f4(i) = (2.0*i)*NP1
    f8(i) = (2.0*i)*NP1
  enddo

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = exp(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_exp(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = exp(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_exp(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'exp',t/CLOCK*NP1,ma4, ma8, avg4, avg8

#define pow(x,y) x**y
  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = pow(f4(i),f4(i))
    enddo
    t1 = V_rdtsc()
    call v_pow(f4, f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = pow(f8(i),f8(i))
    enddo
    t3 = V_rdtsc()
    call v_pow(f8, f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'pow',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  do i = 1, NP
    f4(i) = (2.0*i)
    f8(i) = (2.0*i)
  enddo

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = log(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_log(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = log(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_log(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'log',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = log(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_log35(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = log(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_log35(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'log35',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = sqrt(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_sqrt(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = sqrt(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_sqrt(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'sqrt',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = cbrt(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_cbrt(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = cbrt(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_cbrt(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'cbrt',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = cbrt(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_cbrt35(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = cbrt(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_cbrt35(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'cbrt35',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  do i = 1, NP
    f4(i) = (10.0*i)*NP1
    f8(i) = (10.0*i)*NP1
  enddo
  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = sinh(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_sinh(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = sinh(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_sinh(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'sinh',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  do i = 1, NP
    f4(i) = (10.0*i)*NP1
    f8(i) = (10.0*i)*NP1
  enddo
  t = 999999999999_8
  do j = 1, 10
    t0 = V_rdtsc()
    r4 = 999999.0
    r8 = 999999.0
    do i = 1, NP
      r4a(i) = cosh(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_cosh(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = cosh(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_cosh(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'cosh',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  do i = 1, NP
    f4(i) = (10.0*i)*NP1
    f8(i) = (10.0*i)*NP1
  enddo
  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = asinh(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_asinh(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = asinh(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_asinh(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'asinh',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  do i = 1, NP
    f4(i) = 1.0 + (1.0*i)*NP1
    f8(i) = 1.0 + (1.0*i)*NP1
  enddo
  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = acosh(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_acosh(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = acosh(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_acosh(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'acosh',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  do i = 1, NP
    f4(i) = (10.0*i)*NP1
    f8(i) = (10.0*i)*NP1
  enddo
  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = tanh(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_tanh(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = tanh(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_tanh(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'tanh',t/CLOCK*NP1,ma4, ma8, avg4, avg8

  do i = 1, NP
    f4(i) = (10.0*i)*NP1
    f8(i) = (10.0*i)*NP1
  enddo
  do i = 1, NP
    f4(i) = (0.1*i)*NP1
    f8(i) = (0.1*i)*NP1
  enddo
  t = 999999999999_8
  do j = 1, 10
    r4 = 999999.0
    r8 = 999999.0
    t0 = V_rdtsc()
    do i = 1, NP
      r4a(i) = atanh(f4(i))
    enddo
    t1 = V_rdtsc()
    call v_atanh(f4, r4, NP)
    t2 = V_rdtsc()
    do i = 1, NP
      r8a(i) = atanh(f8(i))
    enddo
    t3 = V_rdtsc()
    call v_atanh(f8, r8, NP)
    t4 = V_rdtsc()
    t(1) = min(t(1),t1-t0)
    t(2) = min(t(2),t2-t1)
    t(3) = min(t(3),t3-t2)
    t(4) = min(t(4),t4-t3)
  enddo
  call V_bit_diff(r4, r4a, NP, mi4, ma4, avg4)
  call V_bit_diff(r8, r8a, NP, mi8, ma8, avg8)
  print 1,'atanh',t/CLOCK*NP1,ma4, ma8, avg4, avg8

1 format(A10,4F10.2,11X,2I6,2F6.2)
777 stop
end program
