program markov_chain
    use random_generator
    use markov_utils
    implicit none
    
    ! Parámetros del problema
    integer, parameter :: max_states = 20, max_steps = 1000
    integer :: n_states, n_steps, n_seeds
    integer :: initial_seed, current_seed, current_step
    integer :: current_state, vector_index, i, k
    
    ! Vectores y matrices
    real :: initial_probability(max_states)
    real :: transition_matrix(max_states, max_states)
    real :: current_probabilities(max_states)
    real :: history(max_states, 0:max_steps) = 0.0
    
    ! Generador de números aleatorios
    real :: random_vector(max_steps + 25)
    real :: random_value
    
    ! Configuración inicial
    n_states = 6
    initial_seed = 1347
    n_steps = 50
    n_seeds = 100000
    
    ! Lectura de datos
    call read_probability_vector("Borratxo-P0.dat", initial_probability, n_states)
    call read_transition_matrix("Borratxo-W.dat", transition_matrix, n_states)
    
    ! Inicialización del generador
    call rng_initialize(initial_seed, random_vector)
    
    ! Bucle principal sobre semillas
    seed_loop: do current_seed = initial_seed, initial_seed + (n_seeds - 1)
        ! Generación de números aleatorios
        call random_number_array(random_vector, n_steps + 1)
        
        vector_index = 1
        random_value = random_vector(vector_index)
        vector_index = vector_index + 1
        
        ! Estado inicial
        current_state = discrete_random_variable(random_value, n_states, initial_probability)
        history(current_state, 0) = history(current_state, 0) + 1.0
        
        ! Evolución de la cadena de Markov
        step_loop: do current_step = 1, n_steps
            current_probabilities = transition_matrix(current_state, :)
            
            random_value = random_vector(vector_index)
            vector_index = vector_index + 1
            
            current_state = discrete_random_variable(random_value, n_states, current_probabilities)
            history(current_state, current_step) = history(current_state, current_step) + 1.0
        end do step_loop
    end do seed_loop
    
    ! Normalización de resultados
    call normalize_histogram(history(:, 0:n_steps), n_seeds)
    
    ! Escritura de resultados
    call write_results("Markov.out", history, n_states, n_steps)
    
    print *, "Simulación completada exitosamente"
    
contains

    subroutine write_results(filename, histogram, n_states, n_steps)
        character(len=*), intent(in) :: filename
        real, intent(in) :: histogram(:,:)
        integer, intent(in) :: n_states, n_steps
        integer :: file_unit, k, i
        
        open(newunit=file_unit, file=filename)
        
        do k = 0, n_steps
            write(file_unit, '(i4, 20(1x, f7.5))') k, (histogram(i, k), i = 1, n_states)
        end do
        
        close(file_unit)
    end subroutine write_results

end program markov_chain