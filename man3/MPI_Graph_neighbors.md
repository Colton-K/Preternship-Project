# Name

MPI_Graph_neighbors  - Returns the neighbors of a node associated
with a graph topology.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Graph_neighbors(MPI_Comm comm, int rank, int maxneighbors,
    int neighbors[])
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_GRAPH_NEIGHBORS(COMM, RANK, MAXNEIGHBORS, NEIGHBORS, IERROR)
    INTEGER    COMM, RANK, MAXNEIGHBORS, NEIGHBORS(*), IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Graph_neighbors(comm, rank, maxneighbors, neighbors, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, INTENT(IN) :: rank, maxneighbors
    INTEGER, INTENT(OUT) :: neighbors(maxneighbors)
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `comm` : Communicator with graph topology (handle).
* `rank` : Rank of process in group of comm (integer).
* `maxneighbors` : Size of array neighbors (integer).

# Output Parameters

* `neighbors` : Ranks of processes that are neighbors to specified process (array of
integers).
* `IERROR` : Fortran only: Error status (integer).
```


# Description

Example: Suppose that `comm` is a `comm`unicator with a shuffle-exchange
topology. The group has 2n members. Each process is labeled by a(1),
..., a(n) with a(i) E{0,1}, and has three neighbors: exchange (a(1),
..., a(n) = a(1), ..., a(n-1), a(n) (a = 1 - a), shuffle (a(1), ...,
a(n)) = a(2), ..., a(n), a(1), and unshuffle (a(1), ..., a(n)) = a(n),
a(1), ..., a(n-1). The graph adjacency list is illustrated below for
n=3.
                    exchange        shuffle        unshuffle
        node        neighbors(1)    neighbors(2)    neighbors(3)
Suppose that the `comm`unicator `comm` has this topology associated with it.
The following code fragment cycles through the three types of neighbors
and performs an appropriate permutation for each.
    C  assume: each process has stored a real number A.
    C  extract neighborhood information
          CALL MPI_COMM_RANK(comm, myrank, ierr)
          CALL MPI_GRAPH_NEIGHBORS(comm, myrank, 3, neighbors, ierr)
    C  perform exchange permutation
          CALL MPI_SENDRECV_REPLACE(A, 1, MPI_REAL, neighbors(1), 0,
         +     neighbors(1), 0, comm, status, ierr)
    C  perform shuffle permutation
          CALL MPI_SENDRECV_REPLACE(A, 1, MPI_REAL, neighbors(2), 0,
         +     neighbors(3), 0, comm, status, ierr)
    C  perform unshuffle permutation
          CALL MPI_SENDRECV_REPLACE(A, 1, MPI_REAL, neighbors(3), 0,
         +     neighbors(2), 0, comm, status, ierr)

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

[`MPI_Graph_neighbors_count(3)`](./?file=MPI_Graph_neighbors_count.md)
