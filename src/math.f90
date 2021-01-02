! 
! Module for some mathematical algorithms.
! 

module math
    implicit none
    
contains
    
    !  
    !  name: gcDenom
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
    
end module



































