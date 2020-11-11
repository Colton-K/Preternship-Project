# Name

`MPI_Win_complete` - Completes an RMA access epoch on win started by
a call to `MPI_Win_start`

# Syntax

## C Syntax

```c
#include <mpi.h>

MPI_Win_complete(MPI_Win win)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_COMPLETE(WIN, IERROR)
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_complete(win, ierror)
    TYPE(MPI_Win), INTENT(IN) :: win
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `win` : Window object (handle).

# Output Parameters

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_complete` is a one-sided MPI communication synchronization call,
completing an RMA access epoch on `win` started by a call to
`MPI_Win_start`. `MPI_Win_complete` enforces the completion of preceding RMA
calls at the origin and not at the target. A put or accumulate call may
not have completed at the target when it has completed at the origin.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Win_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

# See Also

[`MPI_Win_start(3)`](./?file=MPI_Win_start.md)
