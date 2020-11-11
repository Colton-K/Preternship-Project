# Name

`MPI_Group_excl` - Produces a group by reordering an existing group
and taking only unlisted members.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Group_excl(MPI_Group group, int n, const int ranks[],

    MPI_Group *newgroup)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_GROUP_EXCL(GROUP, N, RANKS, NEWGROUP, IERROR)
    INTEGER    GROUP, N, RANKS(*), NEWGROUP, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Group_excl(group, n, ranks, newgroup, ierror)
    TYPE(MPI_Group), INTENT(IN) :: group
    INTEGER, INTENT(IN) :: n, ranks(n)
    TYPE(MPI_Group), INTENT(OUT) :: newgroup
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `group` : Group (handle).
* `n` : Number of elements in array ranks (integer).
* `ranks` : Array of integer ranks in group not to appear in newgroup.

# Output Parameters

* `newgroup` : New group derived from above, preserving the order defined by group
(handle).
* `IERROR` : Fortran only: Error status (integer).
```


# Description

The function `MPI_Group_excl` creates a `group` of processes new`group` that
is obtained by deleting from `group` those processes with ranks
ranks[0], ... ranks[n-1]. The ordering of processes in `newgroup` is
ide`n`tical to the orderi`n`g i`n` group. Each of the `n` eleme`n`ts of ra`n`ks must
be a valid rank in `group` and all elements must be distinct; otherwise,
the call is erro`n`eous. If `n` = 0, the`n` `n`ewgroup is ide`n`tical to group.

# Note

Currently, each of the `ranks` to exclude must be a valid rank in the
group and all elements must be distinct or the function is erroneous.
This restriction is per the draft.

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

[`MPI_Group_range_excl(3)`](./?file=MPI_Group_range_excl.md)
[`MPI_Group_free(3)`](./?file=MPI_Group_free.md)
