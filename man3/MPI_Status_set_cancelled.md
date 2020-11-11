# Name

`MPI_Status_set_cancelled` - Sets status to indicate a request has
been canceled.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Status_set_cancelled(MPI_Status *status, int flag)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_STATUS_SET_CANCELLED(STATUS, FLAG, IERROR)

    INTEGER    STATUS(MPI_STATUS_SIZE), IERROR 
    LOGICAL FLAG
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Status_set_cancelled(status, flag, ierror)
    TYPE(MPI_Status), INTENT(INOUT) :: status
    LOGICAL, INTENT(OUT) :: flag
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `status` : Status with which to associate cancel flag (status).

# Input Parameter

* `flag` : If true, indicates request was canceled (logical).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

If `flag` is set to true, then a subsequent call to
`MPI_Test_cancelled(status`, flag) will also return `flag` = true;
otherwise it will return false.

# Notes

Users are advised not to reuse the `status` fields for values other than
those for which they were intended. Doing so may lead to unexpected
results when using the `status` object. For example, calling
`MPI_Get_elements` may cause an error if the value is out of range, or it
may be impossible to detect such an error. The `extra_state` argument
provided with a generalized request can be used to return information
that does not logically belong in status. Furthermore, modifying the
values in a `status` set internally by MPI, such as `MPI_Recv`, may lead to
unpredictable results and is strongly discouraged.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
