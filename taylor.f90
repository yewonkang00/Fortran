PROGRAM Taylor
   IMPLICIT NONE
   DOUBLE PRECISION :: sum, x, a
   INTEGER :: k
  
   PRINT *, "give x: "
   READ *, x
  
   sum = 0.0d0
   k = 0
   a = 1.0d0 ! initial value of a is the term for n=0.
   DO 
      sum = sum + a
      !     next term is ...
      k = k+1
      a = a * (-x*x) / (2*k*(2*k-1))
      IF (sum + a == sum) EXIT
   ENDDO

   WRITE(*,100) 'The result of cos', x, 'is ', sum
100 FORMAT(A, F10.7)
  
END PROGRAM Taylor
