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

   write(*,*) MAGNE(S, L)

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
end program p3
