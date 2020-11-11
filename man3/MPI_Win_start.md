# Name

`MPI_Win_start` - Starts an RMA access epoch for win

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_start(MPI_Group group, int assert, MPI_Win win)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_START(GROUP, ASSERT, WIN, IERROR)
    INTEGER GROUP, ASSERT, WIN, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_start(group, assert, win, ierror)
    TYPE(MPI_Group), INTENT(IN) :: group
    INTEGER, INTENT(IN) :: assert
    TYPE(MPI_Win), INTENT(IN) :: win
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `group` : The group of target processes (handle).
* `assert` : Program assertion (integer).
* `win` : Window object (handle).

# Output Parameters

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_start` is a one-sided MPI communication synchronization call that
starts an RMA access epoch for win. RMA calls issued on `win` during
this epoch must access only windows at processes in group. Each
process in `group` must issue a matching call to `MPI_Win_post.`
`MPI_Win_start` is allowed to block until the corresponding `MPI_Win_post`
calls have been executed, but is not required to.
The `assert` argument is used to provide assertions on the context of
the call that may be used for various optimizations. (See Section 6.4.4
of the MPI-2 Standard.) A value of `assert` = 0 is always valid. The
following assertion value is supported:
* `MPI_MODE_NOCHECK` : When this value is passed in to this call, the library assumes that
:   When this value is passed in to this call, the library assumes that
    the post call on the target has been called and it is not necessary
    for the library to check to see if such a call has been made.

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

[`MPI_Win_complete(3)`](./?file=MPI_Win_complete.md)
[`MPI_Win_post(3)`](./?file=MPI_Win_post.md)
