# Makefile: 
#        It uses the GNU C++ Compiler with debugging
#        will create executalbe for 2 programs called fixme and fixed
# ----------------------------------------------------------------------
# the compiler
CPP = g++ 
#
# compiler flags
CFLAGS = -L/usr/lib/cmplrs/cxx -DPOSIX_4D9   -w -g
#
# this linker flag will link in the math library libm.a 
LFLAGS = -lm
#
RM = rm -f
#
# -----------------------------------------------------------------------
# Explanation of macros:
#     $< is any dependent file which is out of file1
#     $@ is the target's fullname
#
# add suffix .cpp since it is not a default with make utility 
.SUFFIXES:	.cpp .o
#
#
all:	fixme fixed	
#
fixme:	fixme.cpp 
	g++ -g -o fixme fixme.cpp 
#
fixed: fixed.cpp 
	g++ -g -o fixed fixed.cpp 
#
clean:
	${RM} core fixme fixed log err

