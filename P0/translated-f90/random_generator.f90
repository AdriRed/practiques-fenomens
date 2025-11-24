module random_generator
    implicit none
    private
    public :: rcarin, rcarry, rng_initialize, random_number_array
    
    ! Constantes del módulo
    real, parameter :: TWOM24 = 1.0 / 16777216.0
    integer, parameter :: MAX_RANDOM_SIZE = 100000
    
    ! Variables del estado del generador
    real :: carry = 0.0
    
contains

    subroutine rng_initialize(seed, random_vector)
        integer, intent(in) :: seed
        real, intent(out) :: random_vector(:)
        integer :: ij, kl, i, j, k, l, ii, jj, m
        real :: s, t
        
        ij = seed / 30082
        kl = seed - 30082 * ij
        i = mod(ij / 177, 177) + 2
        j = mod(ij, 177) + 2
        k = mod(kl / 169, 178) + 1
        l = mod(kl, 169)
        
        do ii = 24, 1, -1
            s = 0.0
            t = 0.5
            do jj = 1, 24
                m = mod(mod(i * j, 179) * k, 179)
                i = j
                j = k
                k = m
                l = mod(53 * l + 1, 169)
                if (mod(l * m, 64) >= 32) s = s + t
                t = 0.5 * t
            end do
            random_vector(ii) = s
        end do
        
        carry = 0.0
    end subroutine rng_initialize

    subroutine random_number_array(random_vector, vector_length)
        real, intent(inout) :: random_vector(:)
        integer, intent(in) :: vector_length
        integer :: ivec, i
        real :: uni
        integer, parameter :: in48 = -48
        
        do ivec = 25, vector_length + 24
            uni = random_vector(ivec - 24) - random_vector(ivec - 10) - carry
            if (uni < 0.0) then
                uni = uni + 1.0
                carry = TWOM24
            else
                carry = 0.0
            end if
            
            if (uni == 0.0) then
                uni = random_vector(ivec - 24) * TWOM24
                if (uni == 0.0) uni = 2.0 ** in48
            end if
            
            random_vector(ivec) = uni
        end do
        
        do i = 1, 24
            random_vector(i) = random_vector(vector_length + i)
        end do
    end subroutine random_number_array

    ! Interfaces para mantener compatibilidad
    subroutine rcarin(seed, random_vector)
        integer, intent(in) :: seed
        real, intent(out) :: random_vector(:)
        call rng_initialize(seed, random_vector)
    end subroutine rcarin

    subroutine rcarry(random_vector, vector_length)
        real, intent(inout) :: random_vector(:)
        integer, intent(in) :: vector_length
        call random_number_array(random_vector, vector_length)
    end subroutine rcarry

end module random_generator