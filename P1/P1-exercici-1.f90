program p1
   use mt19937

   implicit none
   integer*4 SEED, iRAND, NRAND
   real*8 x, sum, sum2, sigma, L, jLATTICE
   L = 8
   SEED=541766
   NRAND=40000
   open (unit=11,file='p1-ex1.dat', status = 'unknown')
   call init_genrand(SEED)
   sum=0.0d0
   sum2=0.0d0
   do iRAND=1,NRAND
      x= grnd()
      jLATTICE =floor(x*L)+1
      !valor enter entre 1 i L (el recorregut del genrand_real2() es tancat en 0 i obert en 1)
      write(11,*) iRAND,x,jLATTICE
      sum = sum + x
      sum2=sum2+x*x
   enddo
   close(11)
   sum = sum/real(NRAND)
   sum2=sum2/real(NRAND)
   sigma = dsqrt(sum2-sum*sum)
   write(*,*) sum, sigma
   stop
end
