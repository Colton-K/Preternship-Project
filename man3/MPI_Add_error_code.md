# Name

`MPI_Add_error_code` - Creates a new error code associated with
errorclass

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Add_error_code(int errorclass, int *errorcode)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_ADD_ERROR_CODE(ERRORCLASS, ERRORCODE, IERROR)
    INTEGER  ERRORCLASS, ERRORCODE, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Add_error_code(errorclass, errorcode, ierror)
    INTEGER, INTENT(IN) :: errorclass
    INTEGER, INTENT(OUT) :: errorcode
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `errorclass` : MPI error class (integer).

# Output Parameters

* `errorcode` : Error code returned by an MPI routine or an MPI error class
(integer).
* `IERROR` : Fortran only: Error status (integer).
```


# Description

Creates a new error code associated with `errorclass` and returns its
value in errorcode.

# Notes

No function is provided to free error codes, as it is not expected that
an application will create them in significant numbers.
The value returned is always greater than or equal to `MPI_ERR_LASTCODE.`

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

[`MPI_Add_error_class(3)`](./?file=MPI_Add_error_class.md)
[`MPI_Error_class(3)`](./?file=MPI_Error_class.md)
