program monte_carlo_exponential
    use random_generator
    implicit none
    
    ! Parámetros de la simulación
    integer, parameter :: max_samples = 50000
    integer :: n_samples, seed, j
    real :: random_vector(max_samples + 25)
    
    ! Estadísticas
    real :: sum_samples, sum_squared_samples
    real :: current_mean, current_variance, error_estimate
    real :: final_mean, final_variance, final_error
    
    ! Configuración
    seed = 139177
    n_samples = 10000
    
    ! Inicialización del generador
    call rng_initialize(seed, random_vector)
    call random_number_array(random_vector, n_samples)
    
    sum_samples = 0.0
    sum_squared_samples = 0.0
    
    ! Archivo de salida
    open(13, file='integexp.out')
    
    ! Bucle principal
    do j = 1, n_samples
        sum_samples = sum_samples + exp(random_vector(j))
        sum_squared_samples = sum_squared_samples + exp(random_vector(j)) ** 2
        
        ! Estadísticas en tiempo real
        current_mean = sum_samples / real(j)
        current_variance = sum_squared_samples / real(j) - current_mean ** 2
        error_estimate = 1.96 * sqrt(current_variance) / sqrt(real(j))
        
        write(13, *) j, current_mean, error_estimate
    end do
    
    close(13)
    
    ! Resultados finales
    final_mean = sum_samples / real(n_samples)
    final_variance = sum_squared_samples / real(n_samples) - final_mean ** 2
    final_error = 1.96 * sqrt(final_variance / real(n_samples))
    
    ! Salida por pantalla
    print *, 'Semilla = ', seed
    print *, '<I>    = ', final_mean, ' Valor teórico: ', exp(1.0) - 1.0
    print *, 'V(I)   = ', final_variance
    print *, 'Error  = ', final_error
    
end program monte_carlo_exponential