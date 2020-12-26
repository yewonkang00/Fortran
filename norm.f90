PROGRAM Matrix

   IMPLICIT NONE

   REAL*8 :: ans, A1norm 
 
   INTEGER :: r,c

   WRITE(*, *) 'Enter row and column.'

   WRITE(*, *) 'Row: '

   READ(*, *) r

   WRITE(*, *) 'Col: '

   READ(*, *) c

   WRITE(*, *) 'Enter the matrix.'
   ans = A1norm(r, c)

   write(*,*) 'A1norm : ', int(ans)

END PROGRAM Matrix
 

   real*8 function A1norm(M, N)

      IMPLICIT NONE

      INTEGER :: i, j, M, N

      real*8 A(M, N)

      REAL(8) :: s

      REAL y(20)
       
      Do i = 1, M
         READ *, (A(i, j), j=1,N)
      ENDDO 

      DO i = 1,N
         y(i) = 0
      ENDDO

      DO i=1, N

         DO j=1, M

            y(i) = y(i) + A(j, i)

         END DO

      END DO

 

     DO i=1, N-1

         s = max(y(i), y(i+1))

      END DO


      A1norm = s
      RETURN

   END

