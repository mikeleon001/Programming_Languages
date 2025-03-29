C********************************************************************
C NAME: Mihail Chitorog
C ASGT: Activity 2
C ORGN: CSUB - CMPS 3500
C FILE: neat.f
C DATE: 09/28/2024
C********************************************************************
C     PRIME NUMBER GENERATOR

      PROGRAM PRIME_NUM_GENERATOR
      INTEGER :: MAYBE, IPOSS, L, LIMIT
      REAL :: MAYBER

      PRINT *, 'PRIME NUMBERS FROM 3 TO 100:'

      MAYBE = 3
      LIMIT = 100

C     Loop through odd numbers starting from 3
      DO WHILE (MAYBE <= LIMIT)
         MAYBER = SQRT(REAL(MAYBE))
         L = 0

C        Check if the number is prime
         DO IPOSS = 2, INT(MAYBER)
            IF (MOD(MAYBE, IPOSS) == 0) THEN
               L = 1
               EXIT
            END IF
         END DO

C        Print if the number is prime
         IF (L == 0) THEN
            PRINT '(1H,10X,I7)', MAYBE
         END IF

C        Move to the next odd number
         MAYBE = MAYBE + 2
      END DO

      STOP
      END PROGRAM PRIME_NUM_GENERATOR      
