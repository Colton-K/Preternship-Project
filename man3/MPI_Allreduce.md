# Name

`MPI_Allreduce`, `MPI_Iallreduce` - Combines values from all processes
and distributes the result back to all processes.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Allreduce(const void *sendbuf, void *recvbuf, int count,

                  MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)

int MPI_Iallreduce(const void *sendbuf, void *recvbuf, int count,

                   MPI_Datatype datatype, MPI_Op op, MPI_Comm comm,

                   MPI_Request *request)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_ALLREDUCE(SENDBUF, RECVBUF, COUNT, DATATYPE, OP, COMM, IERROR)
    <type>    SENDBUF(*), RECVBUF(*)
    INTEGER    COUNT, DATATYPE, OP, COMM, IERROR

MPI_IALLREDUCE(SENDBUF, RECVBUF, COUNT, DATATYPE, OP, COMM, REQUEST, IERROR)
    <type>    SENDBUF(*), RECVBUF(*)
    INTEGER    COUNT, DATATYPE, OP, COMM, REQUEST, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierror)
    TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
    TYPE(*), DIMENSION(..) :: recvbuf
    INTEGER, INTENT(IN) :: count
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    TYPE(MPI_Op), INTENT(IN) :: op
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Iallreduce(sendbuf, recvbuf, count, datatype, op, comm, request,
        ierror)
    TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
    TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
    INTEGER, INTENT(IN) :: count
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    TYPE(MPI_Op), INTENT(IN) :: op
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Request), INTENT(OUT) :: request
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `sendbuf` : Starting address of send buffer (choice).
* `count` : Number of elements in send buffer (integer).
* `datatype` : Datatype of elements of send buffer (handle).
* `op` : Operation (handle).
* `comm` : Communicator (handle).

# Output Parameters

* `recvbuf` : Starting address of receive buffer (choice).
* `request` : Request (handle, non-blocking only).
* `IERROR` : Fortran only: Error status (integer).

# Description

Same as `MPI_Reduce` except that the result appears in the receive buffer
of all the group members.
Example 1: A routine that computes the product of a vector and an
array that are distributed across a group of processes and returns the
answer at all nodes (compare with Example 2, with `MPI_Reduce`, below).
    SUBROUTINE PAR_BLAS2(m, n, a, b, c, comm)
    REAL a(m), b(m,n)    ! local slice of array
    REAL c(n)            ! result
    REAL sum(n)
    INTEGER n, comm, i, j, ierr
    ! local sum
    DO j= 1, n
      sum(j) = 0.0
      DO i = 1, m
        sum(j) = sum(j) + a(i)b(i,j)
    ! global sum
    CALL MPI_ALLREDUCE(sum, c, n, MPI_REAL, MPI_SUM, comm, ierr)
    ! return result at all nodes
Example 2: A routine that computes the product of a vector and an
array that are distributed across a group of processes and returns the
answer at node zero.
    SUBROUTINE PAR_BLAS2(m, n, a, b, c, comm)
    REAL a(m), b(m,n)    ! local slice of array
    REAL c(n)            ! result
    REAL sum(n)
    INTEGER n, comm, i, j, ierr
    ! local sum
    DO j= 1, n
      sum(j) = 0.0
      DO i = 1, m
        sum(j) = sum(j) + a(i)b(i,j)
    ! global sum
    CALL MPI_REDUCE(sum, c, n, MPI_REAL, MPI_SUM, 0, comm, ierr)
    ! return result at node zero (and garbage at the other nodes)

# Use Of In-Place Option

When the communicator is an intracommunicator, you can perform an
all-reduce operation in-place (the output buffer is used as the input
buffer). Use the variable `MPI_IN_PLACE` as the value of `sendbuf` at all
processes.
Note that `MPI_IN_PLACE` is a special kind of value; it has the same
restrictions on its use as `MPI_BOTTOM.`
Because the in-place option converts the receive buffer into a
send-and-receive buffer, a Fortran binding that includes INTENT must
mark these as INOUT, not OUT.

# When Communicator Is An Inter-Communicator

When the communicator is an inter-communicator, the reduce operation
occurs in two phases. The data is reduced from all the members of the
first group and received by all the members of the second group. Then
the data is reduced from all the members of the second group and
received by all the members of the first. The operation exhibits a
symmetric, full-duplex behavior.
When the communicator is an intra-communicator, these groups are the
same, and the operation occurs in a single phase.

# Notes On Collective Operations

The reduction functions ( `MPI_Op` ) do not return an error value. As a
result, if the functions detect an error, all they can do is either call
`MPI_Abort` or silently skip the problem. Thus, if you change the error
handler from `MPI_ERRORS_ARE_FATAL` to something else, for example,
`MPI_ERRORS_RETURN` , then no error may be indicated.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
