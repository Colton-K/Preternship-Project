# Name

`MPI_Win_get_group` - Returns a duplicate of the group of the
communicator used to create the window.

# Syntax

## C Syntax

```c
#include <mpi.h>

MPI_Win_get_group(MPI_Win win, MPI_Group *group)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_GET_GROUP(WIN, GROUP, IERROR)
    INTEGER WIN, GROUP, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_get_group(win, group, ierror)
    TYPE(MPI_Win), INTENT(IN) :: win
    TYPE(MPI_Group), INTENT(OUT) :: group
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `win` : Window object (handle).

# Output Parameters

* `group` : Group of processes that share access to the window (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_get_`group`` returns a duplicate of the `group` of the communicator
used to create the window associated with win. The `group` is returned
in group.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
