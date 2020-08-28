
CC = clang
CC = clang

FC = flang
FC = flang

SLEEF_HOME = /opt/sleef

OPT = -O3
CFLAGS = -march=core-avx2 -std=c99 $(OPT)
FFLAGS = -march=core-avx2 $(OPT)

NPTS = 32768

test.Abs : sleef_wrapper.o sleef.inc ftn_sleef_test.F90
	$(FC) $(FFLAGS) ftn_sleef_test.F90 sleef_wrapper.o -I. -DNPTS=${NPTS} -L$(SLEEF_HOME)/lib -lsleef -lm ${LD_SET_RPATH} -o test.Abs

sleef.inc : functions.h
	grep '^//[ !]' functions.h | sed 's:^//::' >sleef.inc

functions.h : FUNCTION.sh  FUNCTION_1_2.sh  FUNCTION_2_1.sh FUNCTIONS.sh
	./FUNCTIONS.sh

sleef_wrapper.o : sleef_wrapper.c functions.h
	$(CC) $(CFLAGS) -c sleef_wrapper.c -I$(SLEEF_HOME)/include -I.

clean :
	rm -f *.Abs functions.h *.o sleef.inc
