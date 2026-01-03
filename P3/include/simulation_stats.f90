module simulation_stats
   use iso_fortran_env, only: int32, real64, int8
   implicit none

   integer, parameter, private :: dp = real64
   integer, parameter, private :: ik = int32
   integer, parameter, private :: bk = int8
   integer, parameter, private :: i2 = 2
   integer, parameter, private :: i4 = 4
   type sim_stats
      real(dp) :: avg_energy, avg_energy_sqrd
      real(dp) :: avg_magne, avg_magne_sqrd, avg_abs_magne
      real(dp) :: var_energy, var_magne
      real(dp) :: temp
      integer(ik) :: sum, L
      real(dp) :: time
   contains
      procedure, public :: add_results
      procedure, public :: normalize
      procedure, public :: calc_var
      procedure, public :: add
   end type sim_stats
contains


   type(sim_stats) function init_stats() result(retval)
      retval%avg_abs_magne = 0
      retval%avg_energy = 0
      retval%var_energy = 0
      retval%avg_energy_sqrd = 0
      retval%avg_magne = 0
      retval%var_magne = 0
      retval%avg_magne_sqrd = 0
      retval%sum = 0
      retval%temp = 0
      retval%L = 0
   end function init_stats
   subroutine add_results(this, mag, energy)
      class(sim_stats), intent(inout) :: this
      real(dp), intent(in) :: mag, energy
      this%sum = this%sum + 1
      this%avg_energy = this%avg_energy + energy
      this%avg_energy_sqrd = this%avg_energy_sqrd + energy*energy
      this%avg_magne = this%avg_magne + mag
      this%avg_abs_magne = this%avg_abs_magne + abs(mag)
      this%avg_magne_sqrd = this%avg_magne_sqrd + mag*mag
   end subroutine add_results

   subroutine normalize(this)
      class(sim_stats), intent(inout) :: this
      this%avg_energy = this%avg_energy /this%sum
      this%avg_energy_sqrd = this%avg_energy_sqrd /this%sum
      this%avg_magne = this%avg_magne /this%sum
      this%avg_abs_magne = this%avg_abs_magne /this%sum
      this%avg_magne_sqrd = this%avg_magne_sqrd /this%sum
   end subroutine normalize

   subroutine calc_var(this)
      class(sim_stats), intent(inout) :: this
      this%var_energy = this%avg_energy_sqrd-this%avg_energy*this%avg_energy
      this%var_magne = this%avg_magne_sqrd-this%avg_magne*this%avg_magne
   end subroutine calc_var

   subroutine add(this, other)
      class(sim_stats), intent(inout) :: this
      class(sim_stats), intent(in) :: other

      this%avg_energy = this%avg_energy + other%avg_energy
      this%avg_energy_sqrd = this%avg_energy_sqrd + other%avg_energy_sqrd
      this%avg_magne = this%avg_magne + other%avg_magne
      this%avg_abs_magne = this%avg_abs_magne + other%avg_abs_magne
      this%avg_magne_sqrd = this%avg_magne_sqrd + other%avg_magne_sqrd
      this%sum = this%sum + 1
      
   end subroutine add

end module simulation_stats
