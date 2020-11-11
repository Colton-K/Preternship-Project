# Name

`MPI_Win_wait` - Completes an RMA exposure epoch started by a call to
`MPI_Win_post` on win

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_wait(MPI_Win win)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_WAIT( WIN, IERROR)
    INTEGER  WIN, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_wait(win, ierror)
    TYPE(MPI_Win), INTENT(IN) :: win
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `win` : Window object (handle).

# Output Parameters

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_wait` is a one-sided MPI communication synchronization call that
completes an RMA exposure epoch started by a call to `MPI_Win_post` on
win. This call matches calls to `MPI_Win_complete(win`) issued by each
of the processes that were granted access to the window during this
epoch. The call to `MPI_Win_wait` blocks until all matching calls to
`MPI_Win_complete` have occurred. This guarantees that all these origin
processes have completed their RMA accesses to the local window. When
the call returns, all these RMA accesses will have completed at the
target window.

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

[`MPI_Win_post(3)`](./?file=MPI_Win_post.md)
