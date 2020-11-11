# Name

`MPI_Win_sync`, - Synchronize the private and public copies of the
window

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_sync (MPI_Win win)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_SYNC(WIN, IERROR)
    INTEGER WIN, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_sync(win, ierror)
    TYPE(MPI_Win), INTENT(IN) :: win
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `win` : Window object (handle).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_sync` synchronizes the private and public window copies of
win. For the purposes of synchronizing the private and public window,
`MPI_Win_sync` has the effect of ending and reopening an access and
exposure epoch on the window (note that it does not actually end an
epoch or complete any pending MPI RMA operations).

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
