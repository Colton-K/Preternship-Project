# Name

`MPI_Group_size` - Returns the size of a group.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Group_size(MPI_Group group, int *size)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_GROUP_SIZE(GROUP, SIZE, IERROR)
    INTEGER    GROUP, SIZE, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Group_size(group, size, ierror)
    TYPE(MPI_Group), INTENT(IN) :: group
    INTEGER, INTENT(OUT) :: size
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `group` : Group (handle).

# Output Parameters

* `size` : Number of processes in the group (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Group_size` returns in `size` the number of processes in the group.
Thus, if `group` = `MPI_GROUP_EMPTY`, then the call will return `size` = 0. On
the other hand, a call with `group` = `MPI_GROUP_NULL` is erroneous.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
