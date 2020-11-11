# Name

`MPI_Neighbor_alltoall`, `MPI_Ineighbor_alltoall` - All processes send
data to neighboring processes in a virtual topology communicator

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Neighbor_alltoall(const void *sendbuf, int sendcount,

    MPI_Datatype sendtype, void *recvbuf, int recvcount,

    MPI_Datatype recvtype, MPI_Comm comm)

int MPI_Ineighbor_alltoall(const void *sendbuf, int sendcount,

    MPI_Datatype sendtype, void *recvbuf, int recvcount,

    MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_NEIGHBOR_ALLTOALL(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
    RECVTYPE, COMM, IERROR)
    <type>    SENDBUF(*), RECVBUF(*)
    INTEGER    SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE
    INTEGER    COMM, IERROR

MPI_INEIGHBOR_ALLTOALL(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
    RECVTYPE, COMM, REQUEST, IERROR)
    <type>    SENDBUF(*), RECVBUF(*)
    INTEGER    SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE
    INTEGER    COMM, REQUEST, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Neighbor_alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount,
        recvtype, comm, ierror)
    TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
    TYPE(*), DIMENSION(..) :: recvbuf
    INTEGER, INTENT(IN) :: sendcount, recvcount
    TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Ineighbor_alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount,
        recvtype, comm, request, ierror)
    TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
    TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
    INTEGER, INTENT(IN) :: sendcount, recvcount
    TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Request), INTENT(OUT) :: request
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `sendbuf` : Starting address of send buffer (choice).
* `sendcount` : Number of elements to send to each process (integer).
* `sendtype` : Datatype of send buffer elements (handle).
* `recvcount` : Number of elements to receive from each process (integer).
* `recvtype` : Datatype of receive buffer elements (handle).
* `comm` : Communicator over which data is to be exchanged (handle).

# Output Parameters

* `recvbuf` : Starting address of receive buffer (choice).
* `request` : Request (handle, non-blocking only).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Neighbor_alltoall` is a collective operation in which all processes
send and receive the same amount of data to each neighbor. The operation
of this routine can be represented as follows, where each process
performs 2n (n being the number of neighbors in communicator comm)
independent point-to-point communications. The neighbors and buffer
layout are determined by the topology of comm.
Example of `MPI_Neighbor_alltoall` semantics for cartesian topologies:
            MPI_Cart_get(comm, maxdims, dims, periods, coords);
            for (dim = 0, i = 0 ; dim < dims ; ++dim) {
                MPI_Cart_shift(comm, dim, 1, &r0, &r1);
                MPI_Isend(sendbuf + i  `sendcount`  extent(sendtype),
                          sendcount, sendtype, r0, ..., comm, ...);
                MPI_Irecv(recvbuf + i  `recvcount`  extent(recvtype),
                          recvcount, recvtype, r0, ..., comm, ...);
                ++i;
                MPI_Isend(sendbuf + i  `sendcount`  extent(sendtype),
                          sendcount, sendtype, r1, ..., comm, &req[i]);
                MPI_Irecv(recvbuf + i  `recvcount`  extent(recvtype),
                          recvcount, recvtype, r1, ..., comm, ...);
                ++i;
            MPI_Waitall (...);
Each process breaks up its local `sendbuf` into n blocks - each
containing `sendcount` elements of type `sendtype` - and divides its
`recvbuf` similarly according to `recvcount` and recvtype. Process j
sends the k-th block of its local `sendbuf` to neighbor k, which places
the data in the j-th block of its local recvbuf. The amount of data
sent must be equal to the amount of data received, pairwise, between
every pair of processes.

# Neighbor Ordering

For a distributed graph topology, created with `MPI_Dist_graph_create,`
the sequence of neighbors in the send and receive buffers at each
process is defined as the sequence returned by `MPI_Dist_graph_neighbors`
for destinations and sources, respectively. For a general graph
topology, created with `MPI_Graph_create`, the order of neighbors in the
send and receive buffers is defined as the sequence of neighbors as
returned by `MPI_Graph_neighbors`. Note that general graph topologies
should generally be replaced by the distributed graph topologies.
For a Cartesian topology, created with `MPI_Cart_create`, the sequence of
neighbors in the send and receive buffers at each process is defined by
order of the dimensions, first the neighbor in the negative direction
and then in the positive direction with displacement 1. The numbers of
sources and destinations in the communication routines are 2ndims with
ndims defined in `MPI_Cart_create`. If a neighbor does not exist, i.e., at
the border of a Cartesian topology in the case of a non-periodic virtual
grid dimension (i.e., periods[...]==false), then this neighbor is
defined to be `MPI_PROC_NULL.`
If a neighbor in any of the functions is `MPI_PROC_NULL`, then the
neighborhood collective communication behaves like a point-to-point
communication with `MPI_PROC_NULL` in this direction. That is, the buffer
is still part of the sequence of neighbors but it is neither
communicated nor updated.

# Notes

The `MPI_IN_PLACE` option for `sendbuf` is not meaningful for this
function.
All arguments on all processes are significant. The `comm` argument, in
particular, must describe the same communicator on all processes. comm
must be either a cartesian, graph, or dist graph communicator.
There are two MPI library functions that are more general than
`MPI_Neighbor_alltoall`. `MPI_Neighbor_alltoallv` allows all-to-all
communication to and from buffers that need not be contiguous; different
processes may send and receive different amounts of data.
`MPI_Neighbor_alltoallw` expands `MPI_Neighbor_alltoallv's` functionality
to allow the exchange of data with different datatypes.

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

[`MPI_Neighbor_alltoallv(3)`](./?file=MPI_Neighbor_alltoallv.md)
[`MPI_Neighbor_alltoallw(3)`](./?file=MPI_Neighbor_alltoallw.md)
[`MPI_Cart_create(3)`](./?file=MPI_Cart_create.md)
[`MPI_Graph_create(3)`](./?file=MPI_Graph_create.md)
[`MPI_Dist_graph_create(3)`](./?file=MPI_Dist_graph_create.md)
[`MPI_Dist_graph_create_adjacent(3)`](./?file=MPI_Dist_graph_create_adjacent.md)
