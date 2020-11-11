# Name

`MPI_Is_thread_main` - Determines if thread called `MPI_Init`

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Is_thread_main(int *flag)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_IS_THREAD_MAIN(FLAG, IERROR)
    LOGICAL    FLAG
    INTEGER    IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Is_thread_main(flag, ierror)
    LOGICAL, INTENT(OUT) :: flag
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Output Parameters

* `flag` : True if calling thread is main thread (boolean).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Is_thread_main` is called by a thread to find out whether the caller
is the main thread (that is, the thread that called `MPI_Init` or
`MPI_Init_thread).`

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

[`MPI_Init(3)`](./?file=MPI_Init.md)
[`MPI_Init_thread(3)`](./?file=MPI_Init_thread.md)
