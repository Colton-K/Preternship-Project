# Name

`MPI_Win_unlock_all` - Completes an RMA access epoch started by a call
to `MPI_Win_lock_all.`

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_unlock_all(MPI_Win win)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_UNLOCK_ALL(WIN, IERROR)
    INTEGER WIN, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_unlock_all(win, ierror)
    TYPE(MPI_Win), INTENT(IN) :: win
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `win` : Window object (handle).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_unlock_all` completes an RMA access epoch started by a call to
`MPI_Win_lock_all`. RMA operations issued during this period will have
completed both at the origin and at the target when the call returns.
Locks are used to protect accesses to the locked target window effected
by RMA calls issued between the lock and unlock call, and to protect
local load/store accesses to a locked local window executed between the
lock and unlock call. Accesses that are protected by an exclusive lock
will not be concurrent at the window site with other accesses to the
same window that are lock protected. Accesses that are protected by a
shared lock will not be concurrent at the window site with accesses
protected by an exclusive lock to the same window.

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

[`MPI_Win_unlock(3)`](./?file=MPI_Win_unlock.md)
[`MPI_Win_lock_all(3)`](./?file=MPI_Win_lock_all.md)