# Name

`MPI_Win_get_name` - Obtains the name of a window.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_get_name(MPI_Win win, char *win_name, int *resultlen)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_GET_NAME(WIN, WIN_NAME, RESULTLEN, IERROR)
    INTEGER WIN, RESULTLEN, IERROR
    CHARACTER*(*) WIN_NAME
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_get_name(win, win_name, resultlen, ierror)
    TYPE(MPI_Win), INTENT(IN) :: win
    CHARACTER(LEN=MPI_MAX_OBJECT_NAME), INTENT(OUT) :: win_name
    INTEGER, INTENT(OUT) :: resultlen
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `win` : Window whose name is to be returned (handle).

# Output Parameters

* `win_name` : the name previously stored on the window, or an empty string if no
such name exists (string).
* `resultlen` : Length of returned name (integer).
* `IERROR` : Fortran only: Error status (integer).
```


# Description


# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
