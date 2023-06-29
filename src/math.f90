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
    !  name: realWholePart
    !  desc: Return the whole part of a real value.
    !  @param r_num: real number
    !  @return The whole part of a real value as an integer.
    !
    integer function realWholePart(r_num) result(whole_part)
        real, intent(in) :: r_num
        
        whole_part = int(r_num)
    end function
    
    !
    !  name: realFracPart
    !  desc: Return the fractional part of a real value.
    !  @param r_num: real number
    !  @return The fractional part of a real value as an integer.
    !
    integer function realFracPart(r_num) result(frac_part)
        real, intent(in) :: r_num
        integer :: n_dec
        integer :: expo
        real :: temp
        
        n_dec = realNumDecimals(r_num)
        expo = 10 ** n_dec

        temp = anint( (r_num - int(r_num)) * expo )
        frac_part = int(temp)
    end function
    
    !
    !  name: realNumDecimals
    !  desc: Return the number of decimals in a real value.
    !  @param r_num: real number
    !  @return The number of decimals as an integer.
    !
    integer function realNumDecimals(r_num) result(n_dec)
        real, intent(in) :: r_num
        real :: eps 
        real :: expo
        
        eps = epsilon( real(0) )
        expo = 1.0
        n_dec = 0
        
        do while (abs( (r_num * expo) - int(r_num * expo) ) > eps)
            n_dec = n_dec + 1
            eps = eps * 10.0
            expo = expo * 10.0
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
    
    !  
    !  name: binaryGap
    !  desc: Finds the length of the longest continuous streak of bits
    !        that have the value 0 (or 1) between two 1- (or 0) bits.
    !        The function must be called with an integer number 
    !        argument.
    !  @param num: integer number, whose bitwise representation is
    !              to be processed. Mandatory parameter.
    !  @param gap_type: logical variable that sets the type of gap to 
    !                   search. When gap==false; gap of 0-bits is 
    !                   searched (default). When gap==true; gap of 
    !                   1-bits is searched. Optional parameter.
    !  @return: The length (as an integer) of the longest streak of 0- 
    !           or 1-bits in the input argument.
    !  
    function binaryGap(num, gap) result(max_gap)
        integer, intent(in) :: num
        logical, intent(in), optional :: gap
        
        logical :: mode
        integer :: i
        integer :: num_bits
        integer :: max_gap
        integer :: bit
        integer :: start_bit_pos
        integer :: end_bit_pos
        integer :: current_gap
        
        ! mode selection; searching for 1-bits or 0-bits.
        if (present(gap)) then
            mode = gap
        else
            mode = .false.
        end if
        
        num_bits = bit_size(num)
        max_gap = 0
        start_bit_pos = -1
        end_bit_pos = -1
        current_gap = 0
        
        do i = 0, (num_bits - 1)
            bit = ibits(num, i, 1)
            
            if (((bit /= 0) .and. (mode .eqv. .false.)) .or. &
                ((bit == 0) .and. (mode .eqv. .true.))) &
            then
                if (start_bit_pos < 0) then
                    start_bit_pos = i
                else if (start_bit_pos >= 0) then
                    start_bit_pos = i
                    
                    if (max_gap < current_gap) then
                        max_gap = current_gap
                    end if
                    
                end if
                current_gap = 0
            end if
            
            
            if (((bit == 0) .and. (mode .eqv. .false.)) .or. &
                ((bit /= 0) .and. (mode .eqv. .true.))) &
            then
                current_gap = current_gap + 1
            end if
            
        end do
        
    end function
    
end module



































