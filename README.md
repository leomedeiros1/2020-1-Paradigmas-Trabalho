# 2020-1-Paradigmas-Trabalho

This repo is a [Fortran 90](https://gcc.gnu.org/onlinedocs/gfortran/) implementation for the GNU [base64](https://github.com/wertarbyte/coreutils/blob/master/src/base64.c) function made to college principles

## Using:

* Compile with ```$ make ```
* Run with ```$ ./mybase64 ```

## Testing

* There are some tests that can be verified with ```$ ./tester.sh ```
* Note that even the `--version` flag will be tested, so errors may occur depending on the version installed on your machine

## Notes

* All output is being directed to `stdout` (different from the original which also uses `stderr`)