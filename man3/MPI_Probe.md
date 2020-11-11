# Name

`MPI_Probe` - Blocking test for a message.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Probe(int source, int tag, MPI_Comm comm, MPI_Status *status)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_PROBE(SOURCE, TAG, COMM, STATUS, IERROR)

    INTEGER    SOURCE, TAG, COMM, STATUS(MPI_STATUS_SIZE), IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Probe(source, tag, comm, status, ierror)
    INTEGER, INTENT(IN) :: source, tag
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Status) :: status
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `source` : Source rank or MPI_ANY_SOURCE (integer).
* `tag` : Tag value or MPI_ANY_TAG (integer).
* `comm` : Communicator (handle).

# Output Parameters

* `status` : Status object (status).
* `IERROR` : Fortran only: Error status (integer).

# Description

The `MPI_Probe` and `MPI_Iprobe` operations allow checking of incoming
messages, without actual receipt of them. The user can then decide how
to receive them, based on the information returned by the probe in the
status variable. For example, the user may allocate memory for the
receive buffer, according to the length of the probed message.
`MPI_Probe` behaves like `MPI_Iprobe` except that it is a blocking call that
returns only after a matching message has been found.
If your application does not need to examine the `status` field, you can
save resources by using the predefined constant `MPI_STATUS_IGNORE` as a
special value for the `status` argument.
The semantics of `MPI_Probe` and `MPI_Iprobe` guarantee progress: If a call
to `MPI_Probe` has been issued by a process, and a send that matches the
probe has been initiated by some process, then the call to `MPI_Probe`
will return, unless the message is received by another concurrent
receive operation (that is executed by another thread at the probing
process). Similarly, if a process busy waits with `MPI_Iprobe` and a
matching message has been issued, then the call to `MPI_Iprobe` will
eventually return flag = true unless the message is received by another
concurrent receive operation.
Example 1: Use blocking probe to wait for an incoming message.
    CALL MPI_COMM_RANK(comm, rank, ierr)
           IF (rank.EQ.0) THEN
                CALL MPI_SEND(i, 1, MPI_INTEGER, 2, 0, comm, ierr)
           ELSE IF(rank.EQ.1) THEN
                CALL MPI_SEND(x, 1, MPI_REAL, 2, 0, comm, ierr)
           ELSE   ! rank.EQ.2
               DO i=1, 2
                                  comm, status, ierr)
                  IF (status(MPI_SOURCE) = 0) THEN
    100                CALL MPI_RECV(i, 1, MPI_INTEGER, 0, 0, status, ierr)
    200                CALL MPI_RECV(x, 1, MPI_REAL, 1, 0, status, ierr)
Each message is received with the right type.
Example 2: A program similar to the previous example, but with a
problem.
    CALL MPI_COMM_RANK(comm, rank, ierr)
           IF (rank.EQ.0) THEN
                CALL MPI_SEND(i, 1, MPI_INTEGER, 2, 0, comm, ierr)
           ELSE IF(rank.EQ.1) THEN
                CALL MPI_SEND(x, 1, MPI_REAL, 2, 0, comm, ierr)
               DO i=1, 2
                                  comm, status, ierr)
                  IF (status(MPI_SOURCE) = 0) THEN
    100                CALL MPI_RECV(i, 1, MPI_INTEGER, MPI_ANY_SOURCE,
                                     0, status, ierr)
    200                CALL MPI_RECV(x, 1, MPI_REAL, MPI_ANY_SOURCE,
                                     0, status, ierr)
We slightly modified Example 2, using `MPI_ANY_SOURCE` as the source
argument in the two receive calls in statements labeled 100 and 200. The
program is now incorrect: The receive operation may receive a message
that is distinct from the message probed by the preceding call to
`MPI_Probe.`

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

[`MPI_Iprobe(3)`](./?file=MPI_Iprobe.md)
[`MPI_Cancel(3)`](./?file=MPI_Cancel.md)
