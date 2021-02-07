program math_test
    use math
    implicit none
    
    integer :: a
    integer :: b
    integer :: gcd
    real :: a2, b2, c2
    real, dimension(2) :: quadRoots
    integer :: nroots
    integer :: i
    
    write(*,'(a)') 'Enter number a: '
    read(*, '(i10)') a
    write(*,'(a)') 'Enter number b: '
    read(*, '(i10)') b
    
    gcd = gcDenom(a, b)
    
    write(*,'(a)') 'Greatest common denominator:'
    write(*,*) gcd
    
    write(*, '(a,i5)') 'Number of digits in a: ', numDigits(a)
    write(*, '(a,i5)') 'Number of digits in b: ', numDigits(b)
    
    write(*, '(a)') 'Enter coefficients a, b, c of polynomial "ax^2 + bx + c = 0":'
    read(*, *) a2, b2, c2
    call quadSolve(a2, b2, c2, nroots, quadRoots)
    
    write(*, '(a,i0)') 'Number of roots found: ', nroots
    do i = 1, nroots
        write(*, '(a,i0,a,f0.4)') ' root ', i, ': ', quadRoots(i)
    end do
    
end program



































