# Name

`MPI_Win_create_keyval` - Creates a keyval for a window.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_create_keyval(MPI_Win_copy_attr_function *win_copy_attr_fn,

    MPI_Win_delete_attr_function *win_delete_attr_fn,
    int *win_keyval, void *extra_state)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_CREATE_KEYVAL(WIN_COPY_ATTR_FN, WIN_DELETE_ATTR_FN,
    WIN_KEYVAL, EXTRA_STATE, IERROR)
    EXTERNAL WIN_COPY_ATTR_FN, WIN_DELETE_ATTR_FN
    INTEGER WIN_KEYVAL, IERROR

    INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_create_keyval(win_copy_attr_fn, win_delete_attr_fn, win_keyval,
        extra_state, ierror)
    PROCEDURE(MPI_Win_copy_attr_function) :: win_copy_attr_fn
    PROCEDURE(MPI_Win_delete_attr_function) :: win_delete_attr_fn
    INTEGER, INTENT(OUT) :: win_keyval
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `win_copy_attr_fn` : Copy callback function for *win_keyval* (function).
* `win_delete_attr_fn` : Delete callback function for *win_keyval* (function).
* `extra_state` : Extra state for callback functions.

# Output Parameters

* `win_keyval` : Key value for future access (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

The argument `win_copy_attr_fn` may be specified as `MPI_WIN_NULL_COPY_FN`
or `MPI_WIN_DUP_FN` from either C or Fortran. `MPI_WIN_NULL_COPY_FN` is a
function that serves only to return `flag` = 0 and `MPI_SUCCESS.`
`MPI_WIN_DUP_FN` is a simple-minded copy function that sets `flag` = 1,
returns the value of `attribute_val_in` in `attribute_val_out`, and
returns `MPI_SUCCESS.`
The argument `win_delete_attr_fn` may be specified as
`MPI_WIN_NULL_DELETE_FN` from either C or Fortran. `MPI_WIN_NULL_DELETE_FN`
is a function that serves only to return `MPI_SUCCESS.`
The C callback functions are:
    typedef int MPI_Win_copy_attr_function(MPI_Win oldwin, int win_keyval,
                 void extra_state, void attribute_val_in,
                 void attribute_val_out, int flag);
and
    typedef int MPI_Win_delete_attr_function(MPI_Win win, int win_keyval,
                 void attribute_val, void extra_state);
The Fortran callback functions are:
and

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the
`EXTRA_STATE` argument only for Fortran 90. FORTRAN 77 users may use the
non-portable syntax
where `MPI_ADDRESS_KIND` is a constant defined in mpif.h and gives the
length of the declared integer in bytes.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
