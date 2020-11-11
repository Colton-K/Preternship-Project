# Name

`MPI_Win_post` - Starts an RMA exposure epoch for the local window
associated with win

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_post(MPI_Group group, int assert, MPI_Win win)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_POST(GROUP, ASSERT, WIN, IERROR)
    INTEGER GROUP, ASSERT, WIN, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_post(group, assert, win, ierror)
    TYPE(MPI_Group), INTENT(IN) :: group
    INTEGER, INTENT(IN) :: assert
    TYPE(MPI_Win), INTENT(IN) :: win
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `group` : The group of origin processes (handle)
* `assert` : Program assertion (integer)
* `win` : Window object (handle)

# Output Parameters

* `IERROR` : Fortran only: Error status (integer).

# Description

Starts an RMA exposure epoch for the local window associated with win.
Only the processes belonging to `group` should access the window with
RMA calls on `win` during this epoch. Each process in `group` must issue
a matching call to `MPI_Win_start`. `MPI_Win_post` does not block.

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

[`MPI_Win_wait(3)`](./?file=MPI_Win_wait.md)
[`MPI_Win_start(3)`](./?file=MPI_Win_start.md)
