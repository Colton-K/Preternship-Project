# Name

`MPI_Comm_get_parent` - Returns the parent intercommunicator of
current spawned process.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Comm_get_parent(MPI_Comm *parent)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_GET_PARENT(PARENT, IERROR)
    INTEGER    PARENT, IERROR 
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Comm_get_parent(parent, ierror)
    TYPE(MPI_Comm), INTENT(OUT) :: parent
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Output Parameters

* `parent` : The parent communicator (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

If a process was started with `MPI_Comm_spawn` or `MPI_Comm_spawn_multiple,`
`MPI_Comm_get_parent` returns the "parent" intercommunicator of the
current process. This `parent` intercommunicator is created implicitly
inside of `MPI_Init` and is the same intercommunicator returned by the
spawn call made in the parents.
If the process was not spawned, `MPI_Comm_get_parent` returns
After the `parent` communicator is freed or disconnected,
`MPI_Comm_get_parent` returns `MPI_COMM_NULL.`

# Notes

`MPI_Comm_get_parent` returns a handle to a single intercommunicator.
Calling `MPI_Comm_get_parent` a second time returns a handle to the same
intercommunicator. Freeing the handle with `MPI_Comm_disconnect` or
`MPI_Comm_free` will cause other references to the intercommunicator to
become invalid (dangling). Note that calling `MPI_Comm_free` on the parent
communicator is not useful.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

# See Also

[`MPI_Comm_spawn(3)`](./?file=MPI_Comm_spawn.md)
[`MPI_Comm_spawn_multiple(3)`](./?file=MPI_Comm_spawn_multiple.md)
