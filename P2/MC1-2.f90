program p3
   use mt19937
   implicit none
   integer(4) :: SEED, i,j, L, MONTECARLO_STEPS
   real(8) TEMP
   integer(2), allocatable :: spins(:, :)
   integer(4), allocatable :: periodic_bound_constraints(:)
   integer :: i_mc, steps, step, spins_count
   real(8) :: last_energy, new_energy, diff_energy, q, q_calc
   real(8) :: cached_exp(-2:2)
   character(32) sL, sTEMP, sMCTOT
   character(100) nom

   CALL GETARG(1 , sL)
   CALL GETARG(2 , sTEMP)
   CALL GETARG(3 , sMCTOT)
   READ (sL,*) L
   READ (sTEMP,*) TEMP
   READ (sMCTOT,*) MONTECARLO_STEPS

   SEED=23456
   call init_genrand(SEED)

   allocate(spins(1:L, 1:L))
   allocate(periodic_bound_constraints(0:L+1))

   spins = init_spins(L)
   periodic_bound_constraints = get_periodic_bound_constraints(L)

   WRITE(NOM,'("SIM-L",I0,"-TEMP",F5.3,"-MCTOT",I0)') L,TEMP,MONTECARLO_STEPS

   spins_count = L*L
   do i = -2, 2
      cached_exp(i) = exp(-real(i*4)/temp)
   end do


   
   open(unit=11, file="files/"//trim(NOM)//".dat", action='write')
   last_energy = energy(spins, L, periodic_bound_constraints)
   do i_mc = 1, montecarlo_steps

      do step = 1, spins_count

         i = floor(grnd()*L)+1
         j = floor(grnd()*L)+1

         diff_energy = 2 * spins(i, j) * (spins(I,periodic_bound_constraints(J + 1)) + spins(I,periodic_bound_constraints(J - 1)) &
            + spins(periodic_bound_constraints(I + 1),J) + spins(periodic_bound_constraints(I - 1),J))

         if (diff_energy <= 0) then
            last_energy = last_energy + diff_energy
            spins(i,j) = -spins(i,j)
         else
            q = grnd()
            q_calc = cached_exp(int(diff_energy)/4)
            if (q < q_calc) then
               last_energy = last_energy + diff_energy
               spins(i,j) = -spins(i,j)
            end if
         end if
      end do
      new_energy = energy(spins, L, periodic_bound_constraints)
      write(*, *) 'Calculated step', i_mc
      write(11, *) i_mc, new_energy, last_energy, magnetization(spins, L)
      last_energy = new_energy
   end do

   close(11)

contains

   function init_spins(L) result(spins)
      integer, intent(in) :: L
      integer*2 spins(1:L,1:L)
      do i=1,L
         do j=1,L
            if (grnd().lt.0.5D0) then
               spins(i,j)=1
            else
               spins(i,j)=-1
            endif
         enddo
      enddo
   end function init_spins

   FUNCTION get_periodic_bound_constraints(L) result(periodic_bound_constraints)
      INTEGER, intent(in) :: L
      INTEGER, dimension(0:L+1) :: periodic_bound_constraints
      periodic_bound_constraints(0) = L
      do i = 1, L
         periodic_bound_constraints(i) = i
      end do
      periodic_bound_constraints(L+1) = 1
   END FUNCTION get_periodic_bound_constraints

   REAL(8) FUNCTION energy(S,L,periodic_bound_constraints) result(ene)
      INTEGER(2) :: S(1:L,1:L)
      INTEGER(4) :: I,J,L
      INTEGER(4) :: periodic_bound_constraints(0:L+1)

      ene=0.0D0

      DO I =1,L
         DO J=1,L
            ene=ene-S(i,j)*S(periodic_bound_constraints(I+1),J)-S(i,j)*S(I, periodic_bound_constraints(J+1))
         ENDDO
      ENDDO
      RETURN
   END function energy

   REAL(8) FUNCTION magnetization(S,L) result(mag)
      INTEGER(2) S(1:L,1:L)
      INTEGER(4) I,J,L
      mag=0
      DO I =1,L
         DO J=1,L
            mag=mag+S(i,j)
         ENDDO
      ENDDO
   END
end program p3
