! 
! Module for some mathematical algorithms.
! 

module math
    implicit none
    
contains
    
    !  
    !  name: gcDenom
    !  desc: Calculates the greatest common denominator of two integer
    !        numbers.
    !  @param a: integer number. 
    !  @param b: integer number. 
    !  @return Greatest common denominator of a and b. 
    !          Function parameters will not be modified.
    !  
    pure function gcDenom(a, b) result(gcd)
        integer, intent(in) :: a
        integer, intent(in) :: b
        integer :: gcd
        integer :: swap
        integer :: remainder
        integer :: a_temp
        integer :: b_temp
        
        gcd = 1
        remainder = 1
        a_temp = a
        b_temp = b
        
        if ( (a_temp < 1) .or. (b_temp < 1) ) then
            gcd = 0
            remainder = 0
        end if
        
        if ( (b_temp > a_temp) .and. (gcd > 0) ) then
            swap = a_temp
            a_temp = b_temp
            b_temp = swap
        end if
        
        do while (remainder > 0)
            remainder = mod(a_temp, b_temp)
            if (remainder /= 0) then
                a_temp = b_temp
                b_temp = remainder
                gcd = remainder
            else
                gcd = b_temp
            end if
        end do
        
    end function
    
    !  
    !  name: numDigits
    !  desc: Calculates the number of digits of an integer value.
    !  @param n: integer number
    !  @return Number of digits in n.
    !  
    integer function numDigits(n) result(d)
        integer, intent(in) :: n
        integer :: n_temp
        
        n_temp = n
        d = 1
        do 
            n_temp = n_temp / 10
            if (n_temp == 0) then
                exit
            end if
            d = d + 1
        end do
        
    end function
    
    ! 
    ! name: quadDiscriminant
    ! desc: Calculates the discriminant of a quadratic equation of the
    !       form "ax² + bx + c = 0".
    ! @param a: second degree term 
    ! @param b: first degree term
    ! @param c: constant term
    ! @return: Discriminant of a quadratic equation of the form 
    !          "ax² + bx + c = 0".
    function quadDiscriminant(a, b, c) result(discr)
        real, intent(in) :: a
        real, intent(in) :: b
        real, intent(in) :: c
        real :: discr
        
        discr = b**2 - 4*a*c
    end function
    
    ! 
    ! name: quadSolve
    ! desc: Finds the real roots of a quadratic equation of the form
    !       "ax² + bx + c = 0".
    ! @param a: second degree term
    ! @param b: first degree term
    ! @param c: constant term
    ! @param num_roots: number of found roots
    ! @param roots: found roots are placed on this array 
    ! 
    subroutine quadSolve(a, b, c, num_roots, roots) 
        real, intent(in) :: a
        real, intent(in) :: b
        real, intent(in) :: c
        integer, intent(out) :: num_roots
        real, dimension(2), intent(out) :: roots
        real :: discr
        real :: tmp
        
        discr = quadDiscriminant(a, b, c)
        num_roots = 0
        
        if (discr < 0.0) then ! No real roots.
            num_roots = 0
            
        else if ((discr > 0.0) .and. (a /= 0.0)) then ! Two real roots.
            num_roots = 2
            roots(1) = (-b + sqrt(discr)) / (2.0*a)
            roots(2) = (-b - sqrt(discr)) / (2.0*a)
            
        else ! One real root
            if (a /= 0.0) then
                num_roots = 1
                roots(1) = (-b) / (2.0*a)
            end if
        end if
        
        ! Swaps the roots into increasing order.
        if (num_roots == 2) then
            if (roots(1) > roots(2)) then
                tmp = roots(1)
                roots(1) = roots(2)
                roots(2) = tmp
            end if
        end if
        
        
    end subroutine
    
end module



































