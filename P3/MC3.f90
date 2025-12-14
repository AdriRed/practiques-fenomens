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

   integer(i4) :: n_seed, i_temp, n_temp_steps, L, N
   real(dp) :: temp_base, temp_step
   integer(i4), allocatable :: periodic_bound_constraints(:)
   integer(ik) :: montecarlo_steps, mc_ini, mc_d
   type(sim_stats), allocatable :: temp_stats(:)


   temp_base = 1.4
   temp_step = 0.01
   n_temp_steps = 200
   L = 32
   N = L*L
   montecarlo_steps = 40000
   mc_ini = 2000
   mc_d = 20
   n_seed = 1

   allocate(periodic_bound_constraints(0:L+1))
   allocate(temp_stats(0:n_temp_steps))

   periodic_bound_constraints = get_periodic_bound_constraints()

!$omp parallel do private(i_temp) schedule(static)
   do i_temp =  0, n_temp_steps
      temp_stats(i_temp) = calc_temp(1.4 + temp_step*i_temp)
   end do
!$omp end parallel do

   open(file='./files/stats-mc3.dat', action='write', unit=12)
   do i_temp = 0, n_temp_steps
      write(12, *) temp_stats(i_temp)%L, temp_stats(i_temp)%temp, temp_stats(i_temp)%sum, &
         temp_stats(i_temp)%avg_energy, temp_stats(i_temp)%avg_energy_sqrd, temp_stats(i_temp)%var_energy, &
         temp_stats(i_temp)%avg_magne, temp_stats(i_temp)%avg_magne_sqrd, temp_stats(i_temp)%var_magne
   end do
   close(unit=12)


contains


   type(sim_stats) function calc_temp(temp) result(retval)
      real(dp), intent(in) :: temp
      real(dp) :: cached_exp(-2:2), spins_energy, mag
      integer(i2), allocatable :: spins(:, :)
      integer(i4) :: i, i_mc, seed
      type(sim_stats) :: stats
      type(mt19937_state) :: rng

      
      allocate(spins(1:L, 1:L))
      spins_energy = 0.0
      retval = init_stats()
      do i = -2, 2
         cached_exp(i) = exp(-real(i*4)/temp)
      end do
      do seed = 1, n_seed
         write(*, *) 'T = ',temp,'; seed = ', seed
         call rng%init_genrand(seed)
         stats = init_stats()
         spins = init_spins(rng)
         do i_mc = 1, montecarlo_steps
            call montecarlo_step(rng, spins, periodic_bound_constraints, spins_energy, cached_exp)

            spins_energy = energy(spins, periodic_bound_constraints)

            if (i_mc > mc_ini .and. mod(i_mc, mc_d) == 0) then
               mag = magnetization(spins)
               call stats%add_results(mag, spins_energy)
            end if
         end do

         call stats%normalize()
         call retval%add(stats)
      end do
      call retval%normalize()
      call retval%calc_var()

      retval%L = L
      retval%temp = temp
      deallocate(spins)
   end function calc_temp

   subroutine montecarlo_step(rng, s, pbc, last_e, exp_cache)
      type(mt19937_state), intent(inout) :: rng
      integer(i2), intent(inout) :: s(1:L, 1:L)
      integer(i4), intent(in) :: pbc(0:L+1)
      real(dp), intent(inout) :: last_e
      real(dp) :: diff_energy
      real(dp), intent(in) :: exp_cache(-2:2)
      real(dp) :: q, q_calc
      integer(i4) :: i, j, step


      do step = 1, N

         i = floor(rng%grnd()*L)+1
         j = floor(rng%grnd()*L)+1

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

   function init_spins(rng) result(s)
      type(mt19937_state), intent(inout) :: rng
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

   FUNCTION get_periodic_bound_constraints() result(pbc)
      INTEGER(ik), dimension(0:L+1) :: pbc
      integer(ik) :: i
      pbc(0) = L
      do i = 1, L
         pbc(i) = i
      end do
      pbc(L+1) = 1
   END FUNCTION get_periodic_bound_constraints

   REAL(dp) FUNCTION energy(S,pbc) result(ene)
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

   REAL(dp) FUNCTION magnetization(S) result(mag)
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
