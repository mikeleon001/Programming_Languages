C********************************************************************
C NAME: Mihail Chitorog
C ASGT: Activity 2
C ORGN: CSUB - CMPS 3500
C FILE: matrixops.f
C DATE: 09/28/2024
C********************************************************************
C This program implements matrix operations:
C    - Addition and subtraction
C    - Multiplication, dotproduct
C    - Transpose, power and a combination of both
C    - scalar times x
C ********************************************************************
C     Define variables as arrays of 2 and 1 dimensions
      REAL m1(5,5), m2(5,5) 
      REAL mat_prod_m1_m2(5,5), mat_prod_m2_m1(5,5)
      REAL prod1(5,5), prod2(5,5), transp1(5,5)
      REAL exp2(5,5), power2(5,5), power5(5,5)
      DOUBLE PRECISION power8(5,5), power12(5,5), op1(5,5)
      REAL transposed_m1(5,5), transposed_m2(5,5)
      REAL transposed_prod_m1_m2(5,5), transposed_prod_m2_m1(5,5)
      REAL mat_prod_m1_v1(5), mat_prod_m2_v1(5)
      REAL v1(5), exp1(5)
      INTEGER SIZE1, SIZE2, SIZE3, I, J, P
      REAL, ALLOCATABLE :: mtemp1(:), mtemp2(:), mtemp3(:)

C     ****************************************
C     Reading m1.in
C     ****************************************
      OPEN(1,FILE='m1.in',ERR=2002)
      SIZE1=0 
C     Getting size of matrix in file
  10  READ(1,*,END=20) 
      SIZE1 = SIZE1 + 1
      GO TO 10

C     if empty exit file
  20  CONTINUE
      IF(SIZE1.EQ.0) GO TO 2002

C     Reposition array to beginning
      REWIND(1)

C     Allocating size of new matrix
      ALLOCATE (mtemp1(1:SIZE1))

C   Set values of A and B
      DO 30 I=1,SIZE1
      READ(1,*) mtemp1(I)
  30  CONTINUE

C     Reshape function from a 1D to 2D array         
      m1 = RESHAPE(mtemp1,(/5, 5/))

C     ****************************************
C     Reading m2.in
C     ****************************************
      OPEN(2,FILE='m2.in',ERR=2002)
      SIZE2=0 

C     Getting size of matrix in file
  40  READ(2,*,END=50) 
      SIZE2 = SIZE2 + 1
      GO TO 40

C     if empty exit file
  50  CONTINUE
      IF(SIZE2.EQ.0) GO TO 2002

C     Reposition array to beginning
      REWIND(2)

C     Allocating size of new matrix
      ALLOCATE (mtemp2(1:SIZE2))

C   Set values of A and B
      DO 60 I=1,SIZE2
      READ(2,*) mtemp2(I)
  60  CONTINUE

C     Reshape function from a 1D to 2D array         
      m2 = RESHAPE(mtemp2,(/5, 5/))

C     ****************************************
C     Reading v1.in
C     ****************************************
      OPEN(3,FILE='v1.in',ERR=2002)
      SIZE3=0 

C     Getting size of matrix in file
  70  READ(3,*,END=80) 
      SIZE3 = SIZE3 + 1
      GO TO 70

C     if empty exit file
  80  CONTINUE
      IF(SIZE3.EQ.0) GO TO 2002

C     Reposition array to beginning
      REWIND(3)

C     Allocating size of new matrix
      ALLOCATE (mtemp3(1:SIZE3))

C   Set values of A and B
      DO 90 I=1,SIZE3
      READ(3,*) mtemp3(I)
  90  CONTINUE

      v1 = mtemp3

C     ****************************************
C     Performing Operations
C     ****************************************

C     MATMUL will perform matrix multiplication
      mat_prod_m1_m2 = matmul(m1, m2)
      mat_prod_m2_m1 = matmul(m2, m1)
      mat_prod_m1_v1 = matmul(m1, v1)
      mat_prod_m2_v1 = matmul(m2, v1)

C     Prod1 will perform addition of m1*m2 + m2*m1
      prod1 = mat_prod_m1_m2 + mat_prod_m2_m1

C     Exp1 will perform addition of m2*v1 + m1*v1
      exp1 = mat_prod_m2_v1 + mat_prod_m1_v1

C     Transposing m1, m2, m1*m2 and m2*m1
      DO I = 1, 5
         DO J = 1, 5
            transposed_m1(J, I) = m1(I, J)
            transposed_m2(J, I) = m2(I, J)
            transposed_prod_m1_m2(J, I) = mat_prod_m1_m2(I, J)
            transposed_prod_m2_m1(J, I) = mat_prod_m2_m1(I, J)
         END DO
      END DO

C     Transp1 will perform the operation
C     transpose(m2*m1) - transpose(m2)*transpose(m1)
      transp1 = transposed_prod_m2_m1 - 
     $(matmul(transposed_m2,transposed_m1))

C     Prod2 will perform (m1*m2)*m1 - (m2*m1)*m2
      prod2 = matmul(mat_prod_m1_m2, m1) - matmul(mat_prod_m2_m1, m2)

C     Exp2 will perform multiplication of transpose(m2)* m2      
      exp2 = matmul(transposed_m2, m2) 

C     Power2 will calculate m1**2
      power2 = matmul(m1, m1)

C     Power5 will calculate m1**5
      power5 = m1

      DO P = 1, 4
         power5 = matmul(power5, m1)
      END DO

C     Power8 will calculate m1**8
      power8 = m1

      DO P = 1, 7
         power8 = matmul(power8, m1)
      END DO

C     Power12 will calculate m1**12
      power12 = m1

      DO P = 1, 11
         power12 = matmul(power12, m1)
      END DO

C     Op1 will calculate power(transpose(m1*m2),25)
     
      op1 = transposed_prod_m1_m2

      DO P = 1, 24
         op1 = matmul(op1, transposed_prod_m1_m2)
      END DO
            
C     ****************************************
C     Writing Outputs
C     ****************************************

C     Printing outputs
      print *
      write(*,*) 'Program to show some matrix and vector operations'
      write(*,*) '*************************************************'
      print *

      write(*,*) ' m1 = '
      write( *, 2000) ((m1(i,j),j=1,5),i=1,5)
      print *

      write(*,*) ' m2 = '
      write( *, 2000)  ((m2(i,j),j=1,5),i=1,5)
      print *

      write(*,*) ' v1 = '
      write( *, 2001) v1
      print *

      write(*,*) ' prod1 = m1 * m2 + m2 * m1 = '
      write(*,2000) ((prod1(i,j),j=1,5),i=1,5)
      print *
      
      write(*,*) ' exp1 = m2 * v1 + m1 * v1 = '
      write(*,2001) exp1
      print *
      
      write(*,*) ' transp1 = transpose(m2*m1) - transpose(m2)*transpose(
     $m1) '
      write( *, 2000) ((transp1(i,j),j=1,5),i=1,5)
      print *

      write(*,*) ' prod2 = (m1*m2)*m1 - (m2*m1)*m2 '
      write(*,2000) ((prod2(i,j),j=1,5),i=1,5)
      print *
      
      write(*,*) ' exp2 = transpose(m2)* m2 = '
      write(*,2000) ((exp2(i,j),j=1,5),i=1,5)
      print *
      
      write(*,*) ' power2 = power(m1, 2) = '
      write( *, 2000) ((power2(i,j),j=1,5),i=1,5)
      print *

      write(*,*) ' power5 = power(m1, 5) = '
      write( *, 2000) ((power5(i,j),j=1,5),i=1,5)
      print *

      write(*,*) ' power8 = power(m1, 8) = '
      write( *, 2000) ((power8(i,j),j=1,5),i=1,5)
      print *

      write(*,*) ' power12 = power(m1, 12) = '
      write( *, 2004) ((power12(i,j),j=1,5),i=1,5)
      print *

      write(*,*) ' op1 = power(transpose(m1*m2),25) '
      write( *, 2003) op1
      print *

      write(*,*) ' Good bye! '
      print *
C     Format output arrays
C     Format output for 5x5 array
 2000 format(5x,5f8.1)

C     Format for huge matrix
 2003 format(5(f25.1, 1x))      

C     Format for large matrix
 2004 format(5x,5f12.1)

C     Format output for 5x1 array
 2001 format ((5x,f8.1))
      stop

C     Erroring out of the file cannot be open or empty
 2002 PRINT *,' Empty or missing input file'
      STOP
      END
