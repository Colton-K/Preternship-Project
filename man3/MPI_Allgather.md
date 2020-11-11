# Name

`MPI_Allgather`, `MPI_Iallgather` - Gathers data from all processes and
distributes it to all processes

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Allgather(const void *sendbuf, int  sendcount,

     MPI_Datatype sendtype, void *recvbuf, int recvcount,

     MPI_Datatype recvtype, MPI_Comm comm)

int MPI_Iallgather(const void *sendbuf, int  sendcount,

     MPI_Datatype sendtype, void *recvbuf, int recvcount,

     MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_ALLGATHER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
        RECVTYPE, COMM, IERROR)
    <type>    SENDBUF (*), RECVBUF (*)
    INTEGER    SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, COMM,
    INTEGER    IERROR

MPI_IALLGATHER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
        RECVTYPE, COMM, REQUEST, IERROR)
    <type>    SENDBUF(*), RECVBUF (*)
    INTEGER    SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, COMM
    INTEGER    REQUEST, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Allgather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
        comm, ierror)
    TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
    TYPE(*), DIMENSION(..) :: recvbuf
    INTEGER, INTENT(IN) :: sendcount, recvcount
    TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Iallgather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
        comm, request, ierror)
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
* `sendcount` : Number of elements in send buffer (integer).
* `sendtype` : Datatype of send buffer elements (handle).
* `recvbuf` : Starting address of recv buffer (choice).
* `recvcount` : Number of elements received from any process (integer).
* `recvtype` : Datatype of receive buffer elements (handle).
* `comm` : Communicator (handle).

# Output Parameters

* `recvbuf` : Address of receive buffer (choice).
* `request` : Request (handle, non-blocking only).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Allgather` is similar to `MPI_Gather`, except that all processes
receive the result, instead of just the root. In other words, all
processes contribute to the result, and all processes receive the
result.
The type signature associated with sendcount, `sendtype` at a process must
be equal to the type signature associated with recvcount, `recvtype` at
any other process.
The outcome of a call to `MPI_Allgather(...`) is as if all processes
executed n calls to
      MPI_Gather(sendbuf,sendcount,sendtype,recvbuf,recvcount,
                 recvtype,root,comm),
for root = 0 , ..., n-1. The rules for correct usage of `MPI_Allgather`
are easily found from the corresponding rules for `MPI_Gather.`
Example: The all-gather version of Example 1 in `MPI_Gather`. Using
`MPI_Allgather`, we will gather 100 ints from every process in the group
to every process.
    MPI_Comm comm;
        int gsize,sendarray[100];
        int rbuf;
        MPI_Comm_size( comm, &gsize);
        rbuf = (int )malloc(gsize100sizeof(int));
        MPI_Allgather( sendarray, 100, MPI_INT, rbuf, 100, MPI_INT, comm);
After the call, every process has the group-wide concatenation of the
sets of data.

# Use Of In-Place Option

When the communicator is an intracommunicator, you can perform an
all-gather operation in-place (the output buffer is used as the input
buffer). Use the variable `MPI_IN_PLACE` as the value of sendbuf. In
this case, `sendcount` and `sendtype` are ignored. The input data of
each process is assumed to be in the area where that process would
receive its own contribution to the receive buffer. Specifically, the
outcome of a call to `MPI_Allgather` that used the in-place option is
identical to the case in which all processes executed `n` calls to
       MPI_ALLGATHER ( MPI_IN_PLACE, 0, MPI_DATATYPE_NULL, recvbuf,
       recvcount, recvtype, root, `comm` )
    for root =0, ... , n-1.
Note that `MPI_IN_PLACE` is a special kind of value; it has the same
restrictions on its use as `MPI_BOTTOM.`
Because the in-place option converts the receive buffer into a
send-and-receive buffer, a Fortran binding that includes INTENT must
mark these as INOUT, not OUT.

# When Communicator Is An Inter-Communicator

When the communicator is an inter-communicator, the gather operation
occurs in two phases. The data is gathered from all the members of the
first group and received by all the members of the second group. Then
the data is gathered from all the members of the second group and
received by all the members of the first. The operation, however, need
not be symmetric. The number of items sent by the processes in first
group need not be equal to the number of items sent by the the processes
in the second group. You can move data in only one direction by giving
`sendcount` a value of 0 for communication in the reverse direction.

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

[`MPI_Allgatherv(3)`](./?file=MPI_Allgatherv.md)
[`MPI_Gather(3)`](./?file=MPI_Gather.md)
