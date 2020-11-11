# Name

`MPI_Type_free_keyval` - Frees a previously created type key value.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Type_free_keyval(int *type_keyval)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_TYPE_FREE_KEYVAL(TYPE_KEYVAL, IERROR)
    INTEGER    TYPE_KEYVAL, IERROR 
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Type_free_keyval(type_keyval, ierror)
    INTEGER, INTENT(INOUT) :: type_keyval
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `type_keyval` : Key value to free (integer).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description


# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

# See Also

[`MPI_Type_create_keyval(3)`](./?file=MPI_Type_create_keyval.md)
