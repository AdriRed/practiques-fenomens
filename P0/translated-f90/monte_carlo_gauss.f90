program monte_carlo_gauss
    use random_generator
    implicit none
    
    ! Parámetros de la simulación
    integer, parameter :: max_samples = 50000
    integer :: n_samples, seed, j, valid_samples
    real :: random_vector(max_samples + 25)
    
    ! Variables para el método Box-Muller
    real :: z1, z2, w, gaussian_sample
    
    ! Estadísticas
    real :: sum_samples, sum_squared_samples
    real :: mean, variance, error_estimate
    
    ! Configuración
    seed = 174177
    n_samples = 20000
    valid_samples = 0
    
    ! Inicialización
    call rng_initialize(seed, random_vector, n_samples)
    call random_number_array(random_vector, n_samples)
    
    sum_samples = 0.0
    sum_squared_samples = 0.0
    
    ! Archivo de salida
    open(13, file='integMCgauss.out')
    
    ! Bucle principal
    sample_loop: do j = 1, n_samples, 2
        z1 = random_vector(j) * 2.0 - 1.0
        z2 = random_vector(j + 1) * 2.0 - 1.0
        w = z1 * z1 + z2 * z2
        
        if (w > 1.0 .or. w == 0.0) cycle sample_loop
        
        valid_samples = valid_samples + 1
        gaussian_sample = z1 * sqrt(-2.0 * log(w) / w)
        
        ! Actualización de estadísticas
        sum_samples = sum_samples + cos(gaussian_sample)
        sum_squared_samples = sum_squared_samples + cos(gaussian_sample) ** 2
        
        ! Cálculo de error
        if (valid_samples > 1) then
            mean = sum_samples / real(valid_samples)
            variance = sum_squared_samples / real(valid_samples) - mean ** 2
            error_estimate = 1.96 * sqrt(variance) / sqrt(real(valid_samples))
            
            write(13, *) valid_samples, mean, error_estimate
        end if
    end do sample_loop
    
    close(13)
    
    print *, "Integración Monte Carlo Gaussiana completada"
    print *, "Muestras válidas: ", valid_samples
    
end program monte_carlo_gauss