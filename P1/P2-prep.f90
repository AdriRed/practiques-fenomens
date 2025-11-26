program p3
   use mt19937
   implicit none
   integer*4 SEED, i,j, L
   PARAMETER (L=96)
   integer*2 S(1:L,1:L)
   SEED=23456
   call init_genrand(SEED)
   do i=1,L
      do j=1,L
         if (grnd().lt.0.5D0) then
            S(i,j)=1
         else
            S(i,j)=-1
         endif
      enddo
   enddo

   write(*,*) ENERG(S, L)

contains

   REAL*8 FUNCTION MAGNE(S,L)
      INTEGER*2 S(1:L,1:L)
      INTEGER*4 I,J,L
      REAL*8 MAG
      MAG=0.0D0
      DO I =1,L
         DO J=1,L
            MAG=MAG+S(i,j)
         ENDDO
      ENDDO
      MAGNE=MAG
      RETURN
   END

   REAL*8 FUNCTION ENERG(S,L)
      INTEGER*2 S(1:L,1:L)
      INTEGER*4 I,J,L, x1, y1, x2, y2
      REAL*8 ENE
      ENE=0.0D0

      DO I =0,(L-1)*(L-1)
         DO J=0,(L-1)*(L-1)
            if (I /= J) then
               x1 = I/L
               y1 = mod(I, L)

               x2 = J/L
               y2 = mod(J, L)

               ENE = ENE- S(x1+1, y1+1)*S(x2+1, y2+1)
            
            endif

         ENDDO
      ENDDO
      ENERG=ENE
      RETURN
   END

end program p3
