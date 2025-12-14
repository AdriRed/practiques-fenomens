program p3
   use mt19937
   implicit none
   integer*4 SEED, i,j, L
   PARAMETER (L=48)
   integer*2 S(1:L,1:L)
   integer*4 PBC(0:L+1)
   SEED=23456
   PBC(0) = L
   PBC(L+1) = 1
   do i = 1, L
      PBC(i) = i
   end do
   call init_genrand(SEED)
   do i=1,L
      do j=1,L
         ! S(i, j) = +1
         if (grnd().lt.0.5D0) then
            S(i,j)=1
         else
            S(i,j)=-1
         endif
      enddo
   enddo

   write(*,*) ENERG(S, L, PBC)

contains

   REAL*8 FUNCTION ENERG(S,L,PBC)
      INTEGER*2 S(1:L,1:L)
      INTEGER*4 I,J,L
      INTEGER*4 PBC(0:L+1)

      REAL*8 ENE
      ENE=0.0D0
      DO I =1,L
         DO J=1,L
            ENE=ENE-S(i,j)*S(PBC(I+1),J)-S(i,j)*S(I, PBC(J+1))
         ENDDO
      ENDDO
      ENERG=ENE
      RETURN
   END
end program p3
