# Name

`MPI_Win_fence` - Synchronizes RMA calls on a window.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_fence(int assert, MPI_Win win)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_FENCE(ASSERT, WIN, IERROR)
    INTEGER ASSERT, WIN, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_fence(assert, win, ierror)
    INTEGER, INTENT(IN) :: assert
    TYPE(MPI_Win), INTENT(IN) :: win
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `assert` : Program assertion (integer).
* `win` : Window object (handle).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_fence` synchronizes RMA calls on win. The call is collective on
the group of win. All RMA operations on `win` originating at a given
process and started before the fence call will complete at that process
before the fence call returns. They will be completed at their target
before the fence call returns at the target. RMA operations on win
started by a process after the fence call returns will access their
target window only after `MPI_Win_fence` has been called by the target
process.
The call completes an RMA access epoch if it was preceded by another
fence call and the local process issued RMA communication calls on win
between these two calls. The call completes an RMA exposure epoch if it
was preceded by another fence call and the local window was the target
of RMA accesses between these two calls. The call starts an RMA access
epoch if it is followed by another fence call and by RMA communication
calls issued between these two fence calls. The call starts an exposure
epoch if it is followed by another fence call and the local window is
the target of RMA accesses between these two fence calls. Thus, the
fence call is equivalent to calls to a subset of post, start, complete,
wait.
A fence call usually entails a barrier synchronization: a process
completes a call to `MPI_Win_fence` only after all other processes in the
group have entered their matching call. However, a call to `MPI_Win_fence`
that is known not to end any epoch (in particular, a call with assert
= `MPI_MODE_NOPRECEDE`) does not necessarily act as a barrier.

# Note

Calls to `MPI_Win_fence` should both precede and follow calls to put, get
or accumulate that are synchronized with fence calls.

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

[`MPI_Win_start(3)`](./?file=MPI_Win_start.md)
[`MPI_Win_post(3)`](./?file=MPI_Win_post.md)
[`MPI_Win_complete(3)`](./?file=MPI_Win_complete.md)
[`MPI_Win_wait(3)`](./?file=MPI_Win_wait.md)
[`MPI_Win_create(3)`](./?file=MPI_Win_create.md)
