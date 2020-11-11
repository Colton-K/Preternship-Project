# Name

`MPI_Win_get_errhandler` - Retrieves the error handler currently
associated with a window.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_get_errhandler(MPI_Win win, MPI_Errhandler *errhandler)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_GET_ERRHANDLER(WIN, ERRHANDLER, IERROR)
    INTEGER WIN, ERRHANDLER, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_get_errhandler(win, errhandler, ierror)
    TYPE(MPI_Win), INTENT(IN) :: win
    TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `win` : Window (handle).

# Output Parameters

* `errhandler` : Error handler currently associated with window (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_get_errhandler` retrieves the error handler currently associated
with a window.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
