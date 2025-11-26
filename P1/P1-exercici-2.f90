program p2
   use mt19937
   implicit none
   integer*4 SEED, i,j, L
   PARAMETER (L=96)
   integer*2 S(1:L,1:L)
   real*8 genrand_real2
   SEED=23456
   open (12, file="P1-configuration.conf")
   call init_genrand(SEED)
   do i=1,L
      do j=1,L
         if (grnd().lt.0.5D0) then
            S(i,j)=1
         else
            S(i,j)=-1
         endif
      enddo
      write(12,*) S(I,:)
   enddo
end program p2
