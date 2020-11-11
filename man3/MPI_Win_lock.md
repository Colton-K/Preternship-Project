# Name

`MPI_Win_lock` - Starts an RMA access epoch locking access to a
particular rank.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_lock(int lock_type, int rank, int assert, MPI_Win win)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_LOCK(LOCK_TYPE, RANK, ASSERT, WIN, IERROR)
    INTEGER LOCK_TYPE, RANK, ASSERT, WIN, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_lock(lock_type, rank, assert, win, ierror)
    INTEGER, INTENT(IN) :: lock_type, rank, assert
    TYPE(MPI_Win), INTENT(IN) :: win
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `lock_type` : Either MPI_LOCK_EXCLUSIVE or MPI_LOCK_SHARED (state).
* `rank` : Rank of locked window (nonnegative integer).
* `assert` : Program assertion (integer).
* `win` : Window object (handle).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

Starts an RMA access epoch. Locks ensure that only the windows created
by specific processes can be accessed by those processes (and by no
other processes) during that epoch.
Locks are used to protect accesses to the locked target window effected
by RMA calls issued between the lock and unlock call, and to protect
local load/store accesses to a locked local window executed between the
lock and unlock call. Accesses that are protected by an exclusive lock
will not be concurrent at the window site with other accesses to the
same window that are lock protected. Accesses that are protected by a
shared lock will not be concurrent at the window site with accesses
protected by an exclusive lock to the same window.
The `assert` argument is used to provide assertions on the context of
the call that may be used for various optimizations. (See Section 6.4.4
of the MPI-2 Standard.) A value of `assert` = 0 is always valid.

# Notes

In a client/server environment in which clients connect to a server and
create windows that span both the client and the server, if a client or
server that has obtained a lock on such a window and then terminates
abnormally, the server or other clients may hang in a `MPI_Win_lock` call,
failing to notice that the peer MPI job has terminated.

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

[`MPI_Win_lock_all(3)`](./?file=MPI_Win_lock_all.md)
[`MPI_Win_unlock(3)`](./?file=MPI_Win_unlock.md)
