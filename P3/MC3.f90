program p2
   use mt19937_par
   use simulation_stats
   use iso_fortran_env, only: int32, real64, int8
   use omp_lib
   implicit none

   integer, parameter :: dp = real64
   integer, parameter :: ik = int32
   integer, parameter :: bk = int8
   integer, parameter :: i2 = 2
   integer, parameter :: i4 = 4

   integer(i4) :: n_seed, i_temp
   real(dp) :: temps(34)
   integer(ik) :: mc_ini, mc_d, mctot_array(1)
   integer(i4) :: Ls(5), iL, iMCTOT
   character(3) :: sL
   type(sim_stats) :: tstats

   ! Ls = (/8, 16, 24, 32, 48, 64, 96, 128/)
   ! Ls = (/8, 16, 24/)
   Ls = (/32, 48, 64, 96, 128/)
   temps = (/1.4, 1.6, 1.8, 2., 2.1, 2.2, 2.21, 2.22, 2.23, 2.24, 2.25, 2.26, 2.27, 2.28, 2.29, 2.30, &
         2.31, 2.32, 2.33, 2.34, 2.35, 2.36, 2.37, 2.38, 2.39, 2.40, &
         2.4, 2.5, 2.6, 2.7, 2.8, 3., 3.2, 3.4/)
   mctot_array = (/20000/)
   mc_ini = 2000
   mc_d = 100
   n_seed = 150
   sL = ' '
   ! open(file='./files/times.dat', unit=20, action='write')
   do iL = 1, size(Ls)
      write(sL, "(I3)") Ls(iL)
      open(file='./files/stats-'//sL//'-2.dat', unit=20+iL, action='write')
      write(20+iL, *) '# L, T, SUM, <E>, <E^2>, Var E, <M>, <|M|>, <M^2>, Var M'
   end do


   ! open(file='./files/stats-mc3-'//sL//'-2.dat', action='write', unit=12)
   ! write(12, *) '# L, T, SUM, <E>, <E^2>, Var E, <M>, <|M|>, <M^2>, Var M'


   !$omp parallel do private(iL, iMCTOT, i_temp, sL, tstats) schedule(dynamic) collapse(3)
   do iL = 1, size(Ls)
      do iMCTOT = 1, size(mctot_array)
         do i_temp =  1, size(temps)
            tstats = calculate_stats_for_temp(temps(i_temp), Ls(iL), mctot_array(iMCTOT))

            !$omp critical(write_stats)
            write(20+iL, *) tstats%L, tstats%temp, tstats%sum, &
               tstats%avg_energy, tstats%avg_energy_sqrd, tstats%var_energy, &
               tstats%avg_magne, tstats%avg_abs_magne, &
               tstats%avg_magne_sqrd, tstats%var_magne
            flush(20+iL)
            !$omp end critical(write_stats)

         end do
      end do
   end do
   !$omp end parallel do

   ! close(12)
   do iL = 1, size(Ls)
      close(20+iL)
   end do
contains


   type(sim_stats) function calculate_stats_for_temp(temp, L, montecarlo_steps) result(retval)
      real(dp), intent(in) :: temp
      integer(i4), intent(in) :: L
      integer(ik), intent(in) :: montecarlo_steps
      real(dp), allocatable :: cached_exp(:)
      integer(i4), allocatable :: periodic_bound_constraints(:)
      real(dp) :: time1, time2
      integer(i4) :: i, seed
      type(sim_stats) :: stats

      allocate(periodic_bound_constraints(0:L+1), cached_exp(-2:2))

      retval = init_stats()
      periodic_bound_constraints = get_periodic_bound_constraints(L)
      do i = -2, 2
         cached_exp(i) = exp(-real(i*4)/temp)
      end do
      call cpu_time(time1)
      !$omp parallel do private(seed) schedule(dynamic)
      do seed = 400, 400+n_seed-1
         write(*, *) 'L = ',L,', T = ',temp,', MCTOT = ',montecarlo_steps,'; seed = ', seed
         stats = montecarlo_sim(seed, L, montecarlo_steps, periodic_bound_constraints, cached_exp)
         call retval%add(stats)
      end do
      !$omp end parallel do
      call cpu_time(time2)

      call retval%normalize()
      call retval%calc_var()

      retval%L = L
      retval%temp = temp
      retval%time = time2-time1
      deallocate(periodic_bound_constraints, cached_exp)

   end function calculate_stats_for_temp

   type(sim_stats) function montecarlo_sim(seed, L, montecarlo_steps, periodic_bound_constraints, cached_exp) result(retval)
      integer(i4), intent(in) :: L, seed
      integer(ik), intent(in) :: montecarlo_steps
      real(dp), allocatable, intent(in) :: cached_exp(:)

      real(dp) :: spins_energy, mag
      integer(i2), allocatable :: spins(:, :)
      integer(i4), allocatable :: periodic_bound_constraints(:)
      type(mt19937_state) :: rng
      integer(ik) :: i_mc

      allocate(spins(1:L, 1:L))
      spins_energy = 0.0
      call rng%init_genrand(seed)
      retval = init_stats()
      spins = init_spins(rng, L)

      spins_energy = energy(spins, periodic_bound_constraints, L)
      do i_mc = 1, montecarlo_steps
         call montecarlo_step(rng, spins, periodic_bound_constraints, spins_energy, cached_exp, L)
         spins_energy = energy(spins, periodic_bound_constraints, L)
         if (i_mc > mc_ini .and. mod(i_mc, mc_d) == 0) then
            mag = magnetization(spins, L)
            call retval%add_results(mag, spins_energy)
         end if
      end do

      call retval%normalize()
      deallocate(spins)

   end function

   subroutine montecarlo_step(rng, s, pbc, last_e, exp_cache, L)
      type(mt19937_state), intent(inout) :: rng
      integer(i4), intent(in) :: L
      integer(i4), intent(in) :: pbc(0:L+1)
      integer(i2), intent(inout) :: s(1:L, 1:L)
      real(dp), intent(inout) :: last_e
      real(dp) :: diff_energy
      real(dp), intent(in) :: exp_cache(-2:2)
      real(dp) :: q, q_calc
      integer(i4) :: i, j, step, N

      N = L*L
      do step = 1, N

         i = floor(rng%grnd()*L)+1
         j = floor(rng%grnd()*L)+1
         if (i > L) i = L
         if (j > L) j = L

         diff_energy = 2 * s(i, j) * (s(I,pbc(J + 1)) + s(I,pbc(J - 1)) &
            + s(pbc(I + 1),J) + s(pbc(I - 1),J))

         if (diff_energy <= 0) then
            last_e = last_e + diff_energy
            s(i,j) = -s(i,j)
         else
            q = rng%grnd()
            q_calc = exp_cache(int(diff_energy)/4)
            if (q < q_calc) then
               last_e = last_e + diff_energy
               s(i,j) = -s(i,j)
            end if
         end if
      end do

   end subroutine montecarlo_step

   function init_spins(rng, L) result(s)
      type(mt19937_state), intent(inout) :: rng
      integer(i4), intent(in) :: L
      integer(i2) :: s(1:L,1:L)
      integer(ik) :: i, j
      do i=1,L
         do j=1,L
            if (rng%grnd().lt.0.5D0) then
               s(i,j)=1
            else
               s(i,j)=-1
            endif
         enddo
      enddo
   end function init_spins

   FUNCTION get_periodic_bound_constraints(L) result(pbc)
      integer(i4), intent(in) :: L
      INTEGER(ik), dimension(0:L+1) :: pbc
      integer(ik) :: i
      pbc(0) = L
      do i = 1, L
         pbc(i) = i
      end do
      pbc(L+1) = 1
   END FUNCTION get_periodic_bound_constraints

   REAL(dp) FUNCTION energy(S,pbc, L) result(ene)
      integer(i4), intent(in) :: L
      INTEGER(i2) :: S(1:L,1:L)
      INTEGER(i4) :: I,J
      INTEGER(i4) :: pbc(0:L+1)

      ene=0.0D0

      DO I =1,L
         DO J=1,L
            ene=ene-S(i,j)*S(pbc(I+1),J)-S(i,j)*S(I, pbc(J+1))
         ENDDO
      ENDDO
      RETURN
   END function energy

   REAL(dp) FUNCTION magnetization(S, L) result(mag)
      integer(i4), intent(in) :: L
      INTEGER(i2) S(1:L,1:L)
      INTEGER(i4) I,J
      mag=0
      DO I =1,L
         DO J=1,L
            mag=mag+S(i,j)
         ENDDO
      ENDDO
   END
end program p2
