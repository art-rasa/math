program math_test
    use math
    implicit none
    
    integer :: a
    integer :: b
    integer :: gcd
    
    write(*,'(a)') 'Enter number a: '
    read(*, '(i10)') a
    write(*,'(a)') 'Enter number b: '
    read(*, '(i10)') b
    
    gcd = gcDenom(a, b)
    
    write(*,'(a)') 'Greatest common denominator:'
    write(*,*) gcd
    
    write(*, '(a,i5)') 'Number of digits in a: ', numDigits(a)
    write(*, '(a,i5)') 'Number of digits in b: ', numDigits(b)
    
end program



































