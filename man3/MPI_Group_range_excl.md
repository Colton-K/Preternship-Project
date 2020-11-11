# Name

`MPI_Group_range_excl` - Produces a group by excluding ranges of
processes from an existing group.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Group_range_excl(MPI_Group group, int n, int ranges[][3],

    MPI_Group *newgroup)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_GROUP_RANGE_EXCL(GROUP, N, RANGES, NEWGROUP, IERROR)
    INTEGER    GROUP, N, RANGES(3,*), NEWGROUP, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Group_range_excl(group, n, ranges, newgroup, ierror)
    TYPE(MPI_Group), INTENT(IN) :: group
    INTEGER, INTENT(IN) :: n, ranges(3,n)
    TYPE(MPI_Group), INTENT(OUT) :: newgroup
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `group` : Group (handle).
* `n` : Number of triplets in array ranges (integer).
* `ranges` : A one-dimensional array of integer triplets of the form (first rank,
last rank, stride), indicating the ranks in group of processes to be
excluded from the output group newgroup.
```


# Output Parameters

* `newgroup` : New group derived from above, preserving the order in group
(handle).
* `IERROR` : Fortran only: Error status (integer).
```


# Description

Each computed rank must be a valid rank in `group` and all computed ranks
must be distinct, or else the program is erroneous.
The functionality of this routine is specified to be equivalent to
expanding the array of `ranges` to an array of the excluded ranks and
passing the resulting array of ranks and other arguments to
`MPI_Group_excl`. A call to `MPI_Group_excl` is equivalent to a call to
`MPI_Group_range_excl` with each rank i in ranks replaced by the triplet
(i,i,1) in the argument ranges.

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

[`MPI_Group_excl(3)`](./?file=MPI_Group_excl.md)
[`MPI_Group_free(3)`](./?file=MPI_Group_free.md)
