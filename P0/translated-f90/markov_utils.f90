module markov_utils
    use random_generator, only : random_number_array
    implicit none
    private
    public :: discrete_random_variable, read_probability_vector, &
              read_transition_matrix, normalize_histogram
    
    interface discrete_random_variable
        module procedure discrete_random_variable_scalar
        module procedure discrete_random_variable_array
    end interface

contains

    integer function discrete_random_variable_scalar(random_value, n_states, probabilities) result(selected_state)
        real, intent(in) :: random_value
        integer, intent(in) :: n_states
        real, intent(in) :: probabilities(:)
        real :: cumulative_probability
        integer :: k
        
        cumulative_probability = 0.0
        selected_state = n_states  ! Valor por defecto si no se encuentra
        
        do k = 1, n_states
            cumulative_probability = cumulative_probability + probabilities(k)
            if (random_value < cumulative_probability) then
                selected_state = k
                return
            end if
        end do
    end function discrete_random_variable_scalar

    function discrete_random_variable_array(random_values, n_states, probabilities) result(selected_states)
        real, intent(in) :: random_values(:)
        integer, intent(in) :: n_states
        real, intent(in) :: probabilities(:)
        integer :: selected_states(size(random_values))
        integer :: i
        
        do i = 1, size(random_values)
            selected_states(i) = discrete_random_variable_scalar(random_values(i), n_states, probabilities)
        end do
    end function discrete_random_variable_array

    subroutine read_probability_vector(filename, probability_vector, n_states)
        character(len=*), intent(in) :: filename
        real, intent(out) :: probability_vector(:)
        integer, intent(in) :: n_states
        integer :: i, file_unit
        real :: sum_probabilities
        
        open(newunit=file_unit, file=filename, status='old')
        
        sum_probabilities = 0.0
        do i = 1, n_states
            read(file_unit, *) probability_vector(i)
            sum_probabilities = sum_probabilities + probability_vector(i)
        end do
        
        close(file_unit)
        
        print *, 'Suma de probabilidades iniciales = ', sum_probabilities
    end subroutine read_probability_vector

    subroutine read_transition_matrix(filename, transition_matrix, n_states)
        character(len=*), intent(in) :: filename
        real, intent(out) :: transition_matrix(:,:)
        integer, intent(in) :: n_states
        integer :: i, j, file_unit
        real :: sum_probabilities
        
        open(newunit=file_unit, file=filename, status='old')
        
        do i = 1, n_states
            sum_probabilities = 0.0
            do j = 1, n_states
                read(file_unit, *) transition_matrix(i, j)
                sum_probabilities = sum_probabilities + transition_matrix(i, j)
            end do
            print *, 'Suma de probabilidades de transición desde ', i, ' = ', sum_probabilities
        end do
        
        close(file_unit)
    end subroutine read_transition_matrix

    subroutine normalize_histogram(histogram, n_samples)
        real, intent(inout) :: histogram(:,:)
        integer, intent(in) :: n_samples
        integer :: i, k
        
        do i = 1, size(histogram, 1)
            do k = 1, size(histogram, 2)
                histogram(i, k) = histogram(i, k) / real(n_samples)
            end do
        end do
    end subroutine normalize_histogram

end module markov_utils