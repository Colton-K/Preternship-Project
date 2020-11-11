# Name

`MPI_Win_free` - Frees the window object and returns a null handle.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_free(MPI_Win *win)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_FREE(WIN, IERROR)
    INTEGER WIN, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_free(win, ierror)
    TYPE(MPI_Win), INTENT(INOUT) :: win
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `win` : Window object (handle).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_free` frees the window object `win` and returns a null handle
(equal to `MPI_WIN_NULL)`. This collective call is executed by all
processes in the group associated with win. It can be invoked by a
process only after it has completed its involvement in RMA
communications on window win, that is, the process has called
`MPI_Win_fence`, or called `MPI_Win_unlock` to match a previous call to
`MPI_Win_lock`. When the call returns, the window memory can be freed.

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

[`MPI_Win_fence(3)`](./?file=MPI_Win_fence.md)
[`MPI_Win_unlock(3)`](./?file=MPI_Win_unlock.md)
[`MPI_Win_create(3)`](./?file=MPI_Win_create.md)
