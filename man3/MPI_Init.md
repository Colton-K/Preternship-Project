# Name

`MPI_Init` - Initializes the MPI execution environment

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Init(int *argc, char ***argv)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_INIT(IERROR)
    INTEGER    IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Init(ierror)
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `argc` : C only: Pointer to the number of arguments.
* `argv` : C only: Argument vector.

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

This routine, or `MPI_Init_thread`, must be called before most other MPI
routines are called. There are a small number of errors, such as
`MPI_Initialized` and `MPI_Finalized`. MPI can be initialized at most once;
subsequent calls to `MPI_Init` or `MPI_Init_thread` are erroneous.
All MPI programs must contain a call to `MPI_Init` or `MPI_Init_thread.`
Open MPI accepts the C `argc` and `argv` arguments to main, but neither
modifies, interprets, nor distributes them:
            / declare variables /
            MPI_Init(&argc, &argv);
            / parse arguments /
            / main program /
            MPI_Finalize();

# Notes

The Fortran version does not have provisions for `argc` and `argv` and
takes only IERROR.
The MPI Standard does not say what a program can do before an `MPI_Init`
or after an `MPI_Finalize`. In the Open MPI implementation, it should do
as little as possible. In particular, avoid anything that changes the
external state of the program, such as opening files, reading standard
input, or writing to standard output.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
See the MPI man page for a full list of MPI error codes.

# See Also

[`MPI_Init_thread(3)`](./?file=MPI_Init_thread.md)
[`MPI_Initialized(3)`](./?file=MPI_Initialized.md)
[`MPI_Finalize(3)`](./?file=MPI_Finalize.md)
[`MPI_Finalized(3)`](./?file=MPI_Finalized.md)
