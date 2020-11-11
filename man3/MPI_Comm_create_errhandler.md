# Name

MPI_Comm_create_errhandler  - Creates an error handler that can be
attached to communicators.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Comm_create_errhandler(MPI_Comm_errhandler_function *function,

    MPI_Errhandler *errhandler)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_CREATE_ERRHANDLER(FUNCTION, ERRHANDLER, IERROR)
    EXTERNAL    FUNCTION
    INTEGER    ERRHANDLER, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Comm_create_errhandler(comm_errhandler_fn, errhandler, ierror)
    PROCEDURE(MPI_Comm_errhandler_function) :: comm_errhandler_fn
    TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Deprecated Type Name Note

MPI-2.2 deprecated the `MPI_Comm_errhandler_fn` and
`MPI::Comm::Errhandler_fn` types in favor of `MPI_Comm_errhandler_function`
and `MPI::Comm::Errhandler_function`, respectively. Open MPI supports both
names (indeed, the `_fn` names are typedefs to the `_function` names).

# Input Parameter

* `function` : User-defined error handling procedure (function).

# Output Parameters

* `errhandler` : MPI error handler (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Comm_create_errhandler` creates an error handler that can be attached
to communicators. This `function` is identical to `MPI_Errhandler_create,`
the use of which is deprecated.
In C, the user routine should be a `function` of type
`MPI_Comm_errhandler_function`, which is defined as
        typedef void MPI_Comm_errhandler_function(MPI_Comm , int , ...);
The first argument is the communicator in use. The second is the error
code to be returned by the MPI routine that raised the error. This
typedef replaces `MPI_Handler_function`, the use of which is deprecated.
In Fortran, the user routine should be of this form:

# Errors

Almost all MPI routines return an error value; C routines as the value
of the `function` and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O `function` errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
