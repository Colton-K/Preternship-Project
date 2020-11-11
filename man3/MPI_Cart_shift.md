# Name

MPI_Cart_shift  - Returns the shifted source and destination ranks,
given a shift direction and amount.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Cart_shift(MPI_Comm comm, int direction, int disp,
    int *rank_source, int *rank_dest)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_CART_SHIFT(COMM, DIRECTION, DISP, RANK_SOURCE,
        RANK_DEST, IERROR)
    INTEGER    COMM, DIRECTION, DISP, RANK_SOURCE
    INTEGER    RANK_DEST, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Cart_shift(comm, direction, disp, rank_source, rank_dest, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, INTENT(IN) :: direction, disp
    INTEGER, INTENT(OUT) :: rank_source, rank_dest
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `comm` : Communicator with Cartesian structure (handle).
* `direction` : Coordinate dimension of shift (integer).
* `disp` : Displacement ( > 0: upward shift, < 0: downward shift) (integer).

# Output Parameters

* `rank_source` : Rank of source process (integer).
* `rank_dest` : Rank of destination process (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

If the process topology is a Cartesian structure, an `MPI_Sendrecv`
operation is likely to be used along a coordinate `direction` to perform a
shift of data. As input, `MPI_Sendrecv` takes the rank of a source process
for the receive, and the rank of a destination process for the send. If
the function `MPI_Cart_shift` is called for a Cartesian process group, it
provides the calling process with the above identifiers, which then can
be passed to `MPI_Sendrecv`. The user specifies the coordinate direction
and the size of the step (positive or negative). The function is local.
The `direction` argument indicates the dimension of the shift, i.e., the
coordinate whose value is modified by the shift. The coordinates are
numbered from 0 to ndims-1, where ndims is the number of dimensions.
Note: The `direction` argument is in the range [0, n-1] for an
n-dimensional Cartesian mesh.
Depending on the periodicity of the Cartesian group in the specified
coordinate direction, `MPI_Cart_shift` provides the identifiers for a
circular or an end-off shift. In the case of an end-off shift, the value
`MPI_PROC_NULL` may be returned in `rank_source` or `rank_dest`, indicating
that the source or the destination for the shift is out of range.
Example: The communicator, comm, has a two-dimensional, periodic,
Cartesian topology associated with it. A two-dimensional array of REALs
is stored one element per process, in variable A. One wishes to skew
this array, by shifting column i (vertically, i.e., along the column) by
i steps.
      C find process rank
            CALL MPI_COMM_RANK(comm, rank, ierr)
      C find Cartesian coordinates
            CALL MPI_CART_COORDS(comm, rank, maxdims, coords,
                                 ierr)
      C compute shift source and destination
            CALL MPI_CART_SHIFT(comm, 0, coords(2), source,
                                dest, ierr)
      C skew array
            CALL MPI_SENDRECV_REPLACE(A, 1, MPI_REAL, dest, 0,
                                      source, 0, comm, status,
                                      ierr)

# Note

In Fortran, the dimension indicated by DIRECTION = i has DIMS(i+1)
nodes, where DIMS is the array that was used to create the grid. In C,
the dimension indicated by `direction` = i is the dimension specified by
dims[i].

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.