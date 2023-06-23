! 
! Routines for testing out the procedures in the 'math' module.
! 
program math_test
    use math
    implicit none
    
    integer :: selection
    
    print *, '-----program math_test-----'
    print *, 'Select feature to test:'
    print *, '  1: gcDenom()'
    print *, '  2: numDigits()'
    print *, '  3: quadSolve()'
    print *, '  4: binaryGap()'
    print *, '  5: realNumDecimals()'
    print *, '  6: realFracPart()'
    print *, '  0: test all'
    read(*, '(i3)') selection
    
    select case (selection)
    case (1)
        call testGreatestCommonDenominator()
    
    case (2)
        call testNumDigits()
    
    case (3)
        call testQuadratics()
    
    case (4)
        call testBinaryGap()
    
    case (5)
        call testRealNumDecimals()
        
    case (6)
        call testRealFracPart()
        
    case (0)
        call testGreatestCommonDenominator()
        call testNumDigits()
        call testQuadratics()
        call testBinaryGap()
        call testRealNumDecimals()
    end select
    
contains
    
    subroutine testNumDigits()
        integer :: a
        integer :: b
        
        print *, ''
        print *, '-----testing numDigits()-----'
        
        write(*,'(a)') 'Enter number a: '
        read(*, '(i10)') a
        write(*,'(a)') 'Enter number b: '
        read(*, '(i10)') b
        write(*, '(a,i5)') 'Number of digits in a: ', numDigits(a)
        write(*, '(a,i5)') 'Number of digits in b: ', numDigits(b)
    end subroutine
    
    subroutine testGreatestCommonDenominator()
        integer :: a
        integer :: b
        integer :: gcd
        
        print *, ''
        print *, '-----testing gcDenom()-----'
        
        write(*,'(a)') 'Enter number a: '
        read(*, '(i10)') a
        write(*,'(a)') 'Enter number b: '
        read(*, '(i10)') b
        
        gcd = gcDenom(a, b)
    
        write(*,'(a)') 'Greatest common denominator:'
        write(*,*) gcd
    end subroutine
    
    subroutine testQuadratics()
        real :: a2, b2, c2
        real, dimension(2) :: quadRoots
        integer :: nroots
        integer :: i
        
        print *, ''
        print *, '-----testing quadSolve()-----'
        
        write(*, '(a)') 'Enter coefficients a, b, c of polynomial "ax^2 + bx + c = 0":'
        read(*, *) a2, b2, c2
        call quadSolve(a2, b2, c2, nroots, quadRoots)
        
        write(*, '(a,i0)') 'Number of roots found: ', nroots
        do i = 1, nroots
            write(*, '(a,i0,a,f0.4)') ' root ', i, ': ', quadRoots(i)
        end do
    end subroutine
    
    subroutine testBinaryGap()
        integer :: num
        integer :: gap
        
        print *, ''
        print *, '-----testing binaryGap()-----'
        
        ! 108: 100001000, gap: 4
        num = 108
        gap = binaryGap(num)
        write(*, '(a,i0,a,i2)') 'Binary gap of ', num, ' is: ', gap
        
        ! 0: 000000000, gap: 0
        num = 0
        gap = binaryGap(num)
        write(*, '(a,i0,a,i2)') 'Binary gap of ', num, ' is: ', gap
        
        ! 1067009: 100000100100000000001, gap: 10
        num = 1067009
        gap = binaryGap(num)
        write(*, '(a,i0,a,i2)') 'Binary gap of ', num, ' is: ', gap
        
        ! 1073741825: 1000000000000000000000000000001, gap: 29
        num = 1073741825
        gap = binaryGap(num)
        write(*, '(a,i0,a,i2)') 'Binary gap of ', num, ' is: ', gap
        
        ! -1: 11111111111111111111111111111111, gap: 0
        num = -1
        gap = binaryGap(num)
        write(*, '(a,i0,a,i2)') 'Binary gap of ', num, ' is: ', gap
        
        ! 1: 00000000000000000000000000000001, gap: 0
        num = 1
        gap = binaryGap(num)
        write(*, '(a,i0,a,i2)') 'Binary gap of ', num, ' is: ', gap
        
        ! -12345: 11111111111111111100111111000111, gap: 3
        num = -12345
        gap = binaryGap(num)
        write(*, '(a,i0,a,i2)') 'Binary gap of ', num, ' is: ', gap
        
        ! 2147483646: 01111111111111111111111111111110, gap: 0
        num = 2147483646
        gap = binaryGap(num)
        write(*, '(a,i0,a,i2)') 'Binary gap of ', num, ' is: ', gap
        
        ! 1075855909: 01000000001000000100001000100101, gap: 8
        num = 1075855909
        gap = binaryGap(num)
        write(*, '(a,i0,a,i2)') 'Binary gap of ', num, ' is: ', gap
        
        ! 344457218: 00010100100010000000000000000010, gap: 17
        num = 344457218
        gap = binaryGap(num)
        write(*, '(a,i0,a,i2)') 'Binary gap of ', num, ' is: ', gap
        
        ! 344457218: 00010100100010000000000000000010, 1-bits gap: 1
        num = 344457218
        gap = binaryGap(num, .true.)
        write(*, '(a,i0,a,i2)') 'Binary gap (1-bits) of ', num, ' is: ', gap
        
        ! 2147483646: 01111111111111111111111111111110, 1-bits gap: 30
        num = 2147483646
        gap = binaryGap(num, .true.)
        write(*, '(a,i0,a,i2)') 'Binary gap (1-bits) of ', num, ' is: ', gap
        
        ! -12345: 11111111111111111100111111000111, 1-bits gap: 6
        num = -12345
        gap = binaryGap(num, .true.)
        write(*, '(a,i0,a,i2)') 'Binary gap (1-bits) of ', num, ' is: ', gap
        
        ! 1: 00000000000000000000000000000001, 1-bits gap: 0
        num = 1
        gap = binaryGap(num, .true.)
        write(*, '(a,i0,a,i2)') 'Binary gap (1-bits) of ', num, ' is: ', gap
        
        ! 272888650: 00010000010000111111001101001010, 1-bits gap: 6
        num = 272888650
        gap = binaryGap(num, .true.)
        write(*, '(a,i0,a,i2)') 'Binary gap (1-bits) of ', num, ' is: ', gap
        
        ! -291533576: 11101110100111111000110011111000, 1-bits gap: 6
        num = -291533576
        gap = binaryGap(num, .true.)
        write(*, '(a,i0,a,i2)') 'Binary gap (1-bits) of ', num, ' is: ', gap
    end subroutine
    
    subroutine testRealNumDecimals()
        real :: r
        integer :: n
        character(len=*), parameter :: res_fmt = '(a,f0.15,a,i0)'
        
        print *, ''
        print *, '-----testing realNumDecimals()-----'
        
        r = 1.23
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 1.234
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 1.235
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 1.236
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 1
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 0
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 0.0002
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 0.00002
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 0.000002
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 0.0000002
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 0.00000002
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 12
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 12.0
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 12.1
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 12.123456
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 12.1234567
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = 12.12345678
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = -1
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
        
        r = -1.9001
        n = realNumDecimals(r)
        write(*, res_fmt) 'Number of decimals of ', r, ' is: ', n
    end subroutine
    
    subroutine testRealFracPart()
        real :: r
        integer :: n
        character(len=*), parameter :: res_fmt = '(a,f0.15,a,i0)'
        
        print *, ''
        print *, '-----testing realFracPart()-----'
        
        r = 1.23
        n = realFracPart(r)
        write(*, res_fmt) 'Fractional part of ', r, ' is: ', n
        
        r = 1.023
        n = realFracPart(r)
        write(*, res_fmt) 'Fractional part of ', r, ' is: ', n
        
        r = 1.0023
        n = realFracPart(r)
        write(*, res_fmt) 'Fractional part of ', r, ' is: ', n
        
        r = 1.00023
        n = realFracPart(r)
        write(*, res_fmt) 'Fractional part of ', r, ' is: ', n
        
        r = 1.000023
        n = realFracPart(r)
        write(*, res_fmt) 'Fractional part of ', r, ' is: ', n
        
        r = 1.0000023
        n = realFracPart(r)
        write(*, res_fmt) 'Fractional part of ', r, ' is: ', n
    end subroutine
end program



































