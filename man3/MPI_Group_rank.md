# Name

`MPI_Group_rank` - Returns the rank of the calling process in the
given group.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Group_rank(MPI_Group group, int *rank)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_GROUP_RANK(GROUP, RANK, IERROR)
    INTEGER    GROUP, RANK, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Group_rank(group, rank, ierror)
    TYPE(MPI_Group), INTENT(IN) :: group
    INTEGER, INTENT(OUT) :: rank
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `group` : Group (handle).

# Output Parameters

* `rank` : Rank of the calling process in group, or MPI_UNDEFINED if the
process is not a member (integer).
* `IERROR` : Fortran only: Error status (integer).
```


# Description

`MPI_Group_`rank`` returns as the output parameter ``rank`` the `rank` of the
calling process in `group`. If the process is not a member of `group` then
`MPI_UNDEFINED` is returned.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
