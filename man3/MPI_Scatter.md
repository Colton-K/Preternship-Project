# Name

`MPI_Scatter`, `MPI_Iscatter` - Sends data from one task to all tasks in
a group.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Scatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype,

    void *recvbuf, int recvcount, MPI_Datatype recvtype, int root,

    MPI_Comm comm)

int MPI_Iscatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype,

    void *recvbuf, int recvcount, MPI_Datatype recvtype, int root,

    MPI_Comm comm, MPI_Request *request)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_SCATTER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
        RECVTYPE, ROOT, COMM, IERROR)
    <type>    SENDBUF(*), RECVBUF(*)
    INTEGER    SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, ROOT
    INTEGER    COMM, IERROR

MPI_ISCATTER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
        RECVTYPE, ROOT, COMM, REQUEST, IERROR)
    <type>    SENDBUF(*), RECVBUF(*)
    INTEGER    SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, ROOT
    INTEGER    COMM, REQUEST, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Scatter(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
        root, comm, ierror)
    TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
    TYPE(*), DIMENSION(..) :: recvbuf
    INTEGER, INTENT(IN) :: sendcount, recvcount, root
    TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Iscatter(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
        root, comm, request, ierror)
    TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
    TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
    INTEGER, INTENT(IN) :: sendcount, recvcount, root
    TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Request), INTENT(OUT) :: request
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `sendbuf` : Address of send buffer (choice, significant only at root).
* `sendcount` : Number of elements sent to each process (integer, significant only
at root).
* `sendtype` : Datatype of send buffer elements (handle, significant only at root).
* `recvcount` : Number of elements in receive buffer (integer).
* `recvtype` : Datatype of receive buffer elements (handle).
* `root` : Rank of sending process (integer).
* `comm` : Communicator (handle).
```


# Output Parameters

* `recvbuf` : Address of receive buffer (choice).
* `request` : Request (handle, non-blocking only).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Scatter` is the inverse operation to `MPI_Gather.`
The outcome is as if the `root` executed n send operations,
        MPI_Send(sendbuf + i  `sendcount`  extent(sendtype), `sendcount`,
                 sendtype, i, ...)
and each process executed a receive,
        MPI_Recv(recvbuf, recvcount, recvtype, i, ...).
An alternative description is that the `root` sends a message with
`MPI_Send(sendbuf`, `sendcount`  n, sendtype, ...). This message
is split into `n` equal segments, the ith segment is sent to the ith
process in the group, and each process receives this message as above.
The send buffer is ignored for all nonroot processes.
The type signature associated with sendcount, `sendtype` at the root
must be equal to the type signature associated with recvcount,
`recvtype` at all processes (however, the type maps may be different).
This implies that the amount of data sent must be equal to the amount of
data received, pairwise between each process and the root. Distinct type
maps between sender and receiver are still allowed.
All arguments to the function are significant on process root, while
on other processes, only arguments recvbuf, recvcount, recvtype,
root, `comm` are significant. The arguments `root` and `comm` must
have identical values on all processes.
The specification of counts and types should not cause any location on
the `root` to be read more than once.
Rationale: Though not needed, the last restriction is imposed so as
to achieve symmetry with `MPI_Gather`, where the corresponding restriction
(a multiple-write restriction) is necessary.
Example: The reverse of Example 1 in the `MPI_Gather` manpage. Scatter
sets of 100 ints from the `root` to each process in the group.
            MPI_Comm comm;
            int gsize,sendbuf;
            int root, rbuf[100];
            MPI_Comm_size(comm, &gsize);
            `sendbuf` = (int )malloc(gsize100sizeof(int));
            MPI_Scatter(sendbuf, 100, MPI_INT, rbuf, 100,
                        MPI_INT, root, comm);

# Use Of In-Place Option

When the communicator is an intracommunicator, you can perform a scatter
operation in-place (the output buffer is used as the input buffer). Use
the variable `MPI_IN_PLACE` as the value of the `root` process recvbuf. In
this case, `recvcount` and `recvtype` are ignored, and the `root` process
sends no data to itself.
Note that `MPI_IN_PLACE` is a special kind of value; it has the same
restrictions on its use as `MPI_BOTTOM.`
Because the in-place option converts the receive buffer into a
send-and-receive buffer, a Fortran binding that includes INTENT must
mark these as INOUT, not OUT.

# When Communicator Is An Inter-Communicator

When the communicator is an inter-communicator, the `root` process in the
first group sends data to all processes in the second group. The first
group defines the `root` process. That process uses `MPI_ROOT` as the value
of its `root` argument. The remaining processes use `MPI_PROC_NULL` as the
value of their `root` argument. All processes in the second group use
the rank of that `root` process in the first group as the value of their
``root`` argument. The receive buffer argument of the `root` process in the
first group must be consistent with the receive buffer argument of the
processes in the second group.

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

[`MPI_Scatterv(3)`](./?file=MPI_Scatterv.md)
[`MPI_Gather(3)`](./?file=MPI_Gather.md)
[`MPI_Gatherv(3)`](./?file=MPI_Gatherv.md)
