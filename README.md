# math

## Introduction

This is a Fortran module containing mathematical procedures. Currently implemented are:

* *gcDenom*: returns the greatest common denominator of two integer numbers.
* *numDigits*: returns the number of digits in an integer number.

The library includes a simple driver/test program. It allows the user to test the operation of the procedures for possible errors.

## Building the library:

* Install [FPM](https://github.com/fortran-lang/fpm) (Fortran Package Manager) and copy it into your $PATH as `fpm`.
* Clone the git repository.
* Execute the command `fpm build` inside the base directory of the repository.
* This library includes a simple test program. It can be easily executed with `fpm test`.

This library was compiled with [GFortran](https://gcc.gnu.org/fortran/) and packaged with [FPM](https://github.com/fortran-lang/fpm).


