# Name

`MPI_Win_create_errhandler` - Creates an error handler for a window.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_create_errhandler(MPI_Win_errhandler_function *function,

    MPI_Errhandler *errhandler)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_CREATE_ERRHANDLER(FUNCTION, ERRHANDLER, IERROR)
    EXTERNAL FUNCTION
    INTEGER ERRHANDLER, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_create_errhandler(win_errhandler_fn, errhandler, ierror)
    PROCEDURE(MPI_Win_errhandler_function) :: win_errhandler_fn
    TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Deprecated Type Name Note

MPI-2.2 deprecated the `MPI_Win_errhandler_fn` and `MPI::Win::Errhandler_fn`
types in favor of `MPI_Win_errhandler_function` and
`MPI::Win::Errhandler_function`, respectively. Open MPI supports both
names (indeed, the `_fn` names are typedefs to the `_function` names).

# Input Parameter

* `function` : User-defined error-handling procedure (function).

# Output Parameters

* `errhandler` : MPI error handler (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_create_errhandler` should be, in C, a `function` of type
`MPI_Win_errhandler_function`, which is defined as
    typedef void MPI_Win_errhandler_function(MPI Win , int , ...);
The first argument is the window in use, the second is the error code to
be returned.
In Fortran, the user routine should be of the form:

# Errors

Almost all MPI routines return an error value; C routines as the value
of the `function` and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O `function` errors. The error handler may be changed with
`MPI_Win_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
