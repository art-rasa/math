# math

## Introduction

This is a Fortran library containing mathematical procedures.

Currently implemented are:

| Procedure | Description |
| --------- | ----------- |
| *gcDenom* | Returns the greatest common denominator of two integer numbers. |
| *numDigits* | Returns the number of digits in an integer number |
| *quadDiscriminant* | Calculates the discriminant of a quadratic equation of the form a*x^2 + b*x + c = 0. |
| *quadSolve* | Finds the real roots of a quadratic equation of the form a*x^2 + b*x + c = 0. |
| *binaryGap* | Computes the number of bits in the longest continuous streak of 0-bits (or alternatively 1-bits) that are surrounded by 1-bits (or alternatively 0-bits) in an integer number. |

The library includes a simple driver/test program. It allows the user to test the operation of the procedures for possible errors.

## Building the library

* Install [FPM](https://github.com/fortran-lang/fpm) (Fortran Package Manager) and copy it into your $PATH as `fpm`.
* Clone the git repository.
* Execute the command `fpm build` inside the base directory of the repository.
* This library includes a simple test program. It can be easily executed with `fpm test`.

This library was compiled with [GFortran](https://gcc.gnu.org/fortran/) and packaged with [FPM](https://github.com/fortran-lang/fpm).

## Note

The results obtained from the procedures contained by this library are not guaranteed to be correct. Use at your own risk. 







