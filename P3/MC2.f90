program p2
   use mt19937
   use simulation_stats
   use iso_fortran_env, only: int32, real64, int8
   implicit none

   integer, parameter :: dp = real64
   integer, parameter :: ik = int32
   integer, parameter :: bk = int8
   integer, parameter :: i2 = 2
   integer, parameter :: i4 = 4



   integer(i4) :: NSEED, i_seed, L, N, MONTECARLO_STEPS
   real(dp) :: TEMP
   integer(i2), allocatable :: spins(:, :)
   integer(i4), allocatable :: periodic_bound_constraints(:)
   integer(ik) :: i_mc, step, spins_count, mc_ini, mc_d
   real(dp) :: spins_energy, q, q_calc, mag
   real(dp) :: cached_exp(-2:2)
   character(4) :: seed_str
   type(sim_stats) :: stats, aggregated_stats

   L = 32
   TEMP = 1.4
   montecarlo_steps = 300
   mc_ini = 1
   mc_d = 1
   NSEED = 1

   allocate(spins(1:L, 1:L))
   allocate(periodic_bound_constraints(0:L+1))


   periodic_bound_constraints = get_periodic_bound_constraints(L)

   spins_count = L*L
   do i_seed = -2, 2
      cached_exp(i_seed) = exp(-real(i_seed*4)/temp)
   end do
   open(file='./files/stats.dat', action='write', unit=12)
   do i_seed = 1000, 1000+NSEED-1
      write(seed_str, "(I4)") i_seed
      stats = init_stats()
      call init_genrand(i_seed)
      spins = init_spins(L)
      open(file='./files/spins-start-'//seed_str//'.dat', unit=13)
      call write_spins(spins, L, 13)
      close(13)
      spins_energy = 0.0
      do i_mc = 1, montecarlo_steps
         call montecarlo_step(L, spins, periodic_bound_constraints, spins_energy, cached_exp)


      end do
      mag = magnetization(spins,L)
      spins_energy = energy(spins, L, periodic_bound_constraints)
      call stats%add_results(mag, spins_energy)
      open(file='./files/spins-end-'//seed_str//'.dat', unit=13)
      call write_spins(spins, L, 13)
      close(13)
      call stats%calc_var()
      ! write(12, *) L, TEMP, stats%sum, stats%avg_energy, stats%avg_energy_sqrd, stats%var_energy, &
      !    stats%avg_magne, stats%avg_magne_sqrd, stats%var_magne
      write(12, *) spins_energy, mag
      ! call stats%normalize()
   end do

   ! write(12, *) L, TEMP, stats%sum, stats%avg_energy, stats%avg_energy_sqrd, stats%var_energy, &
   !    stats%avg_magne, stats%avg_magne_sqrd, stats%var_magne


   close(unit=12)


contains

   subroutine write_spins(s, length, unit)
      integer(ik), intent(in) :: length, unit
      integer(i2), intent(inout) :: s(1:length, 1:length)
      integer(ik) :: i, j
      do i = 1, length
         do j = 1, length
            write(unit, "(I2,A)", advance="no") s(i, j), ' '
         end do
         write(unit, *)
      end do
   end subroutine write_spins

   subroutine montecarlo_step(length, s, pbc, last_e, exp_cache)
      integer(ik), intent(in) :: length
      integer(i2), intent(inout) :: s(1:length, 1:length)
      integer(i4), intent(in) :: pbc(0:length+1)
      integer(i4) :: i, j
      real(dp), intent(inout) :: last_e
      real(dp) :: diff_energy
      real(dp), intent(in) :: exp_cache(-2:2)

      do step = 1, length*length

         i = floor(grnd()*length)+1
         j = floor(grnd()*length)+1

         diff_energy = 2 * s(i, j) * (s(I,pbc(J + 1)) + s(I,pbc(J - 1)) &
            + s(pbc(I + 1),J) + s(pbc(I - 1),J))

         if (diff_energy <= 0) then
            last_e = last_e + diff_energy
            s(i,j) = -s(i,j)
         else
            q = grnd()
            q_calc = exp_cache(int(diff_energy)/4)
            if (q < q_calc) then
               last_e = last_e + diff_energy
               s(i,j) = -s(i,j)
            end if
         end if
      end do

   end subroutine montecarlo_step

   function init_spins(length) result(s)
      integer(ik), intent(in) :: length
      integer(i2) s(1:length,1:length)
      integer(ik) :: i, j
      do i=1,length
         do j=1,length
            if (grnd().lt.0.5D0) then
               s(i,j)=1
            else
               s(i,j)=-1
            endif
         enddo
      enddo
   end function init_spins

   FUNCTION get_periodic_bound_constraints(L) result(pbc)
      INTEGER, intent(in) :: L
      INTEGER, dimension(0:L+1) :: pbc
      integer(i4) :: i, j
      pbc(0) = L
      do i = 1, L
         pbc(i) = i
      end do
      pbc(L+1) = 1
   END FUNCTION get_periodic_bound_constraints

   REAL(dp) FUNCTION energy(S,L,pbc) result(ene)
      INTEGER(i2) :: S(1:L,1:L)
      INTEGER(i4) :: I,J,L
      INTEGER(i4) :: pbc(0:L+1)

      ene=0.0D0

      DO I =1,L
         DO J=1,L
            ene=ene-S(i,j)*S(pbc(I+1),J)-S(i,j)*S(I, pbc(J+1))
         ENDDO
      ENDDO
      RETURN
   END function energy

   REAL(dp) FUNCTION magnetization(S,L) result(mag)
      INTEGER(i2) S(1:L,1:L)
      INTEGER(i4) I,J,L
      mag=0
      DO I =1,L
         DO J=1,L
            mag=mag+S(i,j)
         ENDDO
      ENDDO
   END
end program p2
