# Name

`MPI_Alltoallw`, `MPI_Ialltoallw` - All processes send data of different
types to, and receive data of different types from, all processes

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Alltoallw(const void *sendbuf, const int sendcounts[],

    const int sdispls[], const MPI_Datatype sendtypes[],
    void *recvbuf, const int recvcounts[], const int rdispls[],

    const MPI_Datatype recvtypes[], MPI_Comm comm)

int MPI_Ialltoallw(const void *sendbuf, const int sendcounts[],

    const int sdispls[], const MPI_Datatype sendtypes[],
    void *recvbuf, const int recvcounts[], const int rdispls[],

    const MPI_Datatype recvtypes[], MPI_Comm comm,

    MPI_Request *request)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_ALLTOALLW(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPES,
    RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPES, COMM, IERROR)
    <type>    SENDBUF(*), RECVBUF(*)
    INTEGER    SENDCOUNTS(*), SDISPLS(*), SENDTYPES(*)
    INTEGER    RECVCOUNTS(*), RDISPLS(*), RECVTYPES(*)
    INTEGER    COMM, IERROR

MPI_IALLTOALLW(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPES,
    RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPES, COMM, REQUEST, IERROR)
    <type>    SENDBUF(*), RECVBUF(*)
    INTEGER    SENDCOUNTS(*), SDISPLS(*), SENDTYPES(*)
    INTEGER    RECVCOUNTS(*), RDISPLS(*), RECVTYPES(*)
    INTEGER    COMM, REQUEST, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Alltoallw(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts,
        rdispls, recvtypes, comm, ierror)
    TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
    TYPE(*), DIMENSION(..) :: recvbuf
    INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*),
    rdispls(*)
    TYPE(MPI_Datatype), INTENT(IN) :: sendtypes(*)
    TYPE(MPI_Datatype), INTENT(IN) :: recvtypes(*)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Ialltoallw(sendbuf, sendcounts, sdispls, sendtypes, recvbuf,
        recvcounts, rdispls, recvtypes, comm, request, ierror)
    TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
    TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
    INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), sdispls(*),
    recvcounts(*), rdispls(*)
    TYPE(MPI_Datatype), INTENT(IN), ASYNCHRONOUS :: sendtypes(*),
    recvtypes(*)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Request), INTENT(OUT) :: request
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `sendbuf` : Starting address of send buffer.
* `sendcounts` : Integer array, where entry i specifies the number of elements to
send to rank i.
* `sdispls` : Integer array, where entry i specifies the displacement (in bytes,
offset from *sendbuf*) from which to send data to rank i.
* `sendtypes` : Datatype array, where entry i specifies the datatype to use when
sending data to rank i.
* `recvcounts` : Integer array, where entry j specifies the number of elements to
receive from rank j.
* `rdispls` : Integer array, where entry j specifies the displacement (in bytes,
offset from *recvbuf*) to which data from rank j should be written.
* `recvtypes` : Datatype array, where entry j specifies the datatype to use when
receiving data from rank j.
* `comm` : Communicator over which data is to be exchanged.
```


# Output Parameters

* `recvbuf` : Address of receive buffer.
* `request` : Request (handle, non-blocking only).
* `IERROR` : Fortran only: Error status.

# Description

`MPI_Alltoallw` is a generalized collective operation in which all
processes send data to and receive data from all other processes. It
adds flexibility to `MPI_Alltoallv` by allowing the user to specify the
datatype of individual data blocks (in addition to displacement and
element count). Its operation can be thought of in the following way,
where each process performs 2n (n being the number of processes in
communicator comm) independent point-to-point communications
(including communication with itself).
        MPI_Comm_size(comm, &n);
        for (i = 0, i < n; i++)
            MPI_Send(sendbuf + sdispls[i], sendcounts[i],
                sendtypes[i], i, ..., comm);
        for (i = 0, i < n; i++)
            MPI_Recv(recvbuf + rdispls[i], recvcounts[i],
                recvtypes[i], i, ..., comm);
Process j sends the k-th block of its local `sendbuf` to process k,
which places the data in the j-th block of its local recvbuf.
When a pair of processes exchanges data, each may pass different element
count and datatype arguments so long as the sender specifies the same
amount of data to send (in bytes) as the receiver expects to receive.
Note that process i may send a different amount of data to process j
than it receives from process j. Also, a process may send entirely
different amounts and types of data to different processes in the
communicator.
When the communicator is an inter-communicator, the gather operation
occurs in two phases. The data is gathered from all the members of the
first group and received by all the members of the second group. Then
the data is gathered from all the members of the second group and
received by all the members of the first. The operation exhibits a
symmetric, full-duplex behavior.
The first group defines the root process. The root process uses `MPI_ROOT`
as the value of root. All other processes in the first group use
`MPI_PROC_NULL` as the value of root. All processes in the second group
use the rank of the root process in the first group as the value of
root.
When the communicator is an intra-communicator, these groups are the
same, and the operation occurs in a single phase.

# Use Of In-Place Option

When the communicator is an intracommunicator, you can perform an
all-to-all operation in-place (the output buffer is used as the input
buffer). Use the variable `MPI_IN_PLACE` as the value of sendbuf. In
this case, sendcounts, sdispls, and `sendtypes` are ignored. The
input data of each process is assumed to be in the area where that
process would receive its own contribution to the receive buffer.

# Notes

The specification of counts, types, and displacements should not cause
any location to be written more than once.
All arguments on all processes are significant. The `comm` argument, in
particular, must describe the same communicator on all processes.
The offsets of `sdispls` and `rdispls` are measured in bytes. Compare
this to `MPI_Alltoallv`, where these offsets are measured in units of
`sendtype` and recvtype, respectively.

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

[`MPI_Alltoall(3)`](./?file=MPI_Alltoall.md)
[`MPI_Alltoallv(3)`](./?file=MPI_Alltoallv.md)
