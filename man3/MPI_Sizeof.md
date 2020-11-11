# Name

`MPI_Sizeof` - Returns the size, in bytes, of the given type

# Syntax

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_SIZEOF(X, SIZE, IERROR)
<type>    X
INTEGER    SIZE, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Sizeof(x, size, ierror)
    TYPE(*), DIMENSION(..) :: x
    INTEGER, INTENT(OUT) :: size
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `X` : A Fortran variable of numeric intrinsic type (choice).

# Output Parameters

* `SIZE` : Size of machine representation of that type (integer).
* `IERROR` : Error status (integer).

# Description

`MPI_SIZEOF` returns the size (in bytes) of the machine representation of
the given variable. It is a generic Fortran type and has a Fortran
binding only. This routine is similar to the sizeof builtin in C.
However, if given an array argument, it returns the size of the base
element, not the size of the whole array.

# Notes

This function is not available in C because it is not necessary.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
See the MPI man page for a full list of MPI error codes.
