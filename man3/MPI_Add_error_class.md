# Name

    MPI_Add_error_class - Creates a new error class and returns its value

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Add_error_class(int *errorclass)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_ADD_ERROR_CLASS(ERRORCLASS, IERROR)
    INTEGER    ERRORCLASS, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Add_error_class(errorclass, ierror)
    INTEGER, INTENT(OUT) :: errorclass
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Output Parameters

* `errorclass` : New error class (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

The function `MPI_Add_error_class` creates a new, local error class.

# Notes

Because this function is local, the same value of `errorclass` may not
be returned on all processes that make this call, even if they call the
function concurrently. Thus, same error on different processes may not
cause the same value of `errorclass` to be returned. To reduce the
potential for confusion, `MPI_Add_error_string` may be used on multiple
processes to associate the same error string with the newly created
errorclass. Even though `errorclass` may not be consistent across
processes, using `MPI_Add_error_string` will ensure the error string
associated with it will be the same everywhere.
No function is provided to free error classes, as it is not expected
that an application will create them in significant numbers.
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

[`MPI_Add_error_code(3)`](./?file=MPI_Add_error_code.md)
[`MPI_Add_error_string(3)`](./?file=MPI_Add_error_string.md)
[`MPI_Error_class(3)`](./?file=MPI_Error_class.md)
[`MPI_Error_string(3)`](./?file=MPI_Error_string.md)
