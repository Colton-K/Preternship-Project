# Name

`MPI_Neighbor_allgatherv`, `MPI_Ineighbor_allgatherv` - Gathers and
distributes data from and to all neighbors. Each process may contribute
a different amount of data.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Neighbor_allgatherv(const void *sendbuf, int sendcount,

    MPI_Datatype sendtype, void *recvbuf, const int recvcounts[],

    const int displs[], MPI_Datatype recvtype, MPI_Comm comm)

int MPI_Ineighbor_allgatherv(const void *sendbuf, int sendcount,

    MPI_Datatype sendtype, void *recvbuf, const int recvcounts[],

    const int displs[], MPI_Datatype recvtype, MPI_Comm comm,

        MPI_Request *request)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_NEIGHBOR_ALLGATHERV(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF,
        RECVCOUNT, DISPLS, RECVTYPE, COMM, IERROR)
    <type>    SENDBUF(*), RECVBUF(*)
    INTEGER    SENDCOUNT, SENDTYPE, RECVCOUNT(*),
    INTEGER    DISPLS(*), RECVTYPE, COMM, IERROR

MPI_INEIGHBOR_ALLGATHERV(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF,
        RECVCOUNT, DISPLS, RECVTYPE, COMM, REQUEST, IERROR)
    <type>    SENDBUF(*), RECVBUF(*)
    INTEGER    SENDCOUNT, SENDTYPE, RECVCOUNT(*),
    INTEGER    DISPLS(*), RECVTYPE, COMM,REQUEST, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Neighbor_allgatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts,
        displs, recvtype, comm, ierror)
    TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
    TYPE(*), DIMENSION(..) :: recvbuf
    INTEGER, INTENT(IN) :: sendcount, recvcounts(*), displs(*)
    TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Ineighbor_allgatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts,
        displs, recvtype, comm, request, ierror)
    TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
    TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
    INTEGER, INTENT(IN) :: sendcount
    INTEGER, INTENT(IN), ASYNCHRONOUS :: recvcounts(*), displs(*)
    TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Request), INTENT(OUT) :: request
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `sendbuf` : Starting address of send buffer (choice).
* `sendcount` : Number of elements in send buffer (integer).
* `sendtype` : Datatype of send buffer elements (handle).
* `recvcount` : Integer array (of length group size) containing the number of
elements that are received from each neighbor.
* `displs` : Integer array (of length group size). Entry i specifies the
displacement (relative to recvbuf) at which to place the incoming
data from neighbor i.
* `recvtype` : Datatype of receive buffer elements (handle).
* `comm` : Communicator (handle).
```


# Output Parameters

* `recvbuf` : Address of receive buffer (choice).
* `request` : Request (handle, non-blocking only).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Neighbor_allgatherv` is similar to `MPI_Neighbor_allgather` in that all
processes gather data from all neighbors, except that each process can
send a different amount of data. The block of data sent from the jth
neighbor is received by every neighbor and placed in the jth block of
the buffer. The neighbors and buffer layout is determined by the
topology of comm. recvbuf.
The type signature associated with sendcount, sendtype, at process j
must be equal to the type signature associated with the corresponding
entry in `recvcounts` on neighboring processes.

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
operation.

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

[`MPI_Cart_create(3)`](./?file=MPI_Cart_create.md)
[`MPI_Graph_create(3)`](./?file=MPI_Graph_create.md)
[`MPI_Neighbor_allgather(3)`](./?file=MPI_Neighbor_allgather.md)
[`MPI_Dist_graph_create(3)`](./?file=MPI_Dist_graph_create.md)
