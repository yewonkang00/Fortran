PROGRAM  Primes

   IMPLICIT  NONE

 

   INTEGER  :: Number, Divisor, Count, N, L, R

 

   Count = 0                           ! input is correct. start counting

                                          

   DO Number = 11, 10000, 2              ! try all odd numbers 3, 5, 7, ...


      Divisor = 3                       ! divisor starts with 3

      DO

         IF (Divisor*Divisor > Number .OR. MOD(Number,Divisor) == 0)  EXIT

         Divisor = Divisor + 2          ! if does not evenly divide, next odd

      END DO

 

      IF (Divisor*Divisor > Number) THEN     ! are all divisor exhausted?

         L = Number
         N = Number
         R = 0

         DO

            R = R*10

            R = R+MOD(N,10)

            N = N/10

            IF(N==0) EXIT

         END DO

         IF(R==L) THEN

            Count = Count + 1

            WRITE(*,*)  'Palindromic Prime number #', Count, ': ', Number

         END IF
      END IF
   END DO        

    

END PROGRAM  Primes

