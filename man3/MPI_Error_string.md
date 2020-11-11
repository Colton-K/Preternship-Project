# Name

MPI_Error_string  - Returns a string for a given error code.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Error_string(int errorcode, char *string, int *resultlen)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_ERROR_STRING(ERRORCODE, STRING, RESULTLEN, IERROR)
    INTEGER        ERRORCODE, RESULTLEN, IERROR
    CHARACTER*(*)    STRING
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Error_string(errorcode, string, resultlen, ierror)
    INTEGER, INTENT(IN) :: errorcode
    CHARACTER(LEN=MPI_MAX_ERROR_STRING), INTENT(OUT) :: string
    INTEGER, INTENT(OUT) :: resultlen
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `errorcode` : Error code returned by an MPI routine or an MPI error class.

# Output Parameters

* `string` : Text that corresponds to the errorcode.
* `resultlen` : Length of string.
* `IERROR` : Fortran only: Error status (integer).

# Description

Returns the error `string` associated with an error code or class. The
argument `string` must represent storage that is at least
`MPI_MAX_ERROR_STRING` characters long.
The number of characters actually written is returned in the output
argument, resultlen.

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

[`MPI_Error_class(3)`](./?file=MPI_Error_class.md)
