# Name

`MPI_Win_flush_local`, `MPI_Win_flush_local_all` - Complete all
outstanding RMA operations at both the origin

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_flush_local (int rank, MPI_Win win)

int MPI_Win_flush_local_all (MPI_Win win)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_FLUSH_LOCAL(RANK, WIN, IERROR)
    INTEGER RANK, WIN, IERROR

MPI_WIN_FLUSH_LOCAL_ALL(WIN, IERROR)
    INTEGER WIN, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_flush_local(rank, win, ierror)
    INTEGER, INTENT(IN) :: rank
    TYPE(MPI_Win), INTENT(IN) :: win
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Win_flush_local_all(win, ierror)
    TYPE(MPI_Win), INTENT(IN) :: win
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `rank` : Rank of window (nonnegative integer).
* `win` : Window object (handle).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_flush_local` locally completes at the origin all outstanding
RMA operations initiated by the calling process to the target process
specified by `rank` on the specified window. For example, after this
routine completes, the user may reuse any buffers provided to put, get,
or accumulate operations. `MPI_Win_flush_local_all` locally completes
at the origin all outstanding RMA operations to all targets.
Can only be called from within a passive target epoch.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler
`MPI_ERRORS_RETURN` may be used to cause error values to be returned. Note
that MPI does not guarantee that an MPI program can continue past an
error.

# See Also

[`MPI_Win_lock(3)`](./?file=MPI_Win_lock.md)
[`MPI_Win_lock_all(3)`](./?file=MPI_Win_lock_all.md)
[`MPI_Win_flush(3)`](./?file=MPI_Win_flush.md)
