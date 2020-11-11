# Name

MPI_Dims_create  - Creates a division of processors in a Cartesian
grid.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Dims_create(int nnodes, int ndims, int dims[])
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_DIMS_CREATE(NNODES, NDIMS, DIMS, IERROR)
    INTEGER    NNODES, NDIMS, DIMS(*), IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Dims_create(nnodes, ndims, dims, ierror)
    INTEGER, INTENT(IN) :: nnodes, ndims
    INTEGER, INTENT(INOUT) :: dims(ndims)
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `nnodes` : Number of nodes in a grid (integer).
* `ndims` : Number of Cartesian dimensions (integer).

# In/Out Parameter

* `dims` : Integer array of size ndims specifying the number of nodes in each
dimension.
```


# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

For Cartesian topologies, the function `MPI_Dims_create` helps the user
select a balanced distribution of processes per coordinate direction,
depending on the number of processes in the group to be balanced and
optional constraints that can be specified by the user. One use is to
partition all the processes (the size of `MPI_COMM_WORLD's` group) into
an n-dimensional topology.
The entries in the array `dims` are set to describe a Cartesian grid
with `ndims` dimensions and a total of `nnodes` nodes. The dimensions
are set to be as close to each other as possible, using an appropriate
divisibility algorithm. The caller may further constrain the operation
of this routine by specifying elements of array dims. If dims[i] is
set to a positive number, the routine will not modify the number of
nodes in dimension i; only those entries where dims[i] = 0 are
modified by the call.
Negative input values of dims[i] are erroneous. An error will occur if
nnodes is not a multiple of ((pi) over (i, dims[i] != 0)) dims[i].
For dims[i] set by the call, dims[i] will be ordered in
nonincreasing order. Array `dims` is suitable for use as input to routine
`MPI_Cart_create`. `MPI_Dims_create` is local.
Example:
    dims
    before                    dims
    call        function call        on return
    (0,0)    MPI_Dims_create(6, 2, dims)    (3,2)
    (0,0)    MPI_Dims_create(7, 2, dims)     (7,1)
    (0,3,0)    MPI_Dims_create(6, 3, dims)    (2,3,1)
    (0,3,0)    MPI_Dims_create(7, 3, dims)    erroneous call

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
