# Name

`MPI_Win_delete_attr` - Deletes an attribute from a window.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_delete_attr(MPI_Win win, int win_keyval)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_DELETE_ATTR(WIN, WIN_KEYVAL, IERROR)
    INTEGER WIN, WIN_KEYVAL, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_delete_attr(win, win_keyval, ierror)
    TYPE(MPI_Win), INTENT(IN) :: win
    INTEGER, INTENT(IN) :: win_keyval
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `win` : Window from which the attribute is deleted (handle).

# Input Parameter

* `win_keyval` : Key value (integer).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Notes

Note that it is not defined by the MPI standard what happens if the
`delete_fn` callback invokes other MPI functions. In Open MPI, it is not
valid for `delete_fn` callbacks (or any of their children) to add or
delete attributes on the same object on which the `delete_fn` callback is
being invoked.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
