# Name

`MPI_Comm_rank` - Determines the rank of the calling process in the
communicator.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Comm_rank(MPI_Comm comm, int *rank)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_RANK(COMM, RANK, IERROR)
    INTEGER    COMM, RANK, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Comm_rank(comm, rank, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, INTENT(OUT) :: rank
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `comm` : Communicator (handle).

# Output Parameters

* `rank` : Rank of the calling process in group of comm (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

This function gives the `rank` of the process in the particular
communicator's group. It is equivalent to accessing the communicator's
group with `MPI_Comm_group`, computing the `rank` using `MPI_Group_`rank``, and
then freeing the temporary group via `MPI_Group_free.`
Many programs will be written with the master-slave model, where one
process (such as the rank-zero process) will play a supervisory role,
and the other processes will serve as compute nodes. In this framework,
`MPI_Comm_size` and `MPI_Comm_rank` are useful for determining the roles of
the various processes of a communicator.

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

[`MPI_Comm_group(3)`](./?file=MPI_Comm_group.md)
[`MPI_Comm_size(3)`](./?file=MPI_Comm_size.md)
[`MPI_Comm_compare(3)`](./?file=MPI_Comm_compare.md)
