# Name

`MPI_Isend` - Starts a standard-mode, nonblocking send.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Isend(const void *buf, int count, MPI_Datatype datatype, int dest,

    int tag, MPI_Comm comm, MPI_Request *request)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_ISEND(BUF, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)
    <type>    BUF(*)
    INTEGER    COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Isend(buf, count, datatype, dest, tag, comm, request, ierror)
    TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
    INTEGER, INTENT(IN) :: count, dest, tag
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Request), INTENT(OUT) :: request
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `buf` : Initial address of send buffer (choice).
* `count` : Number of elements in send buffer (integer).
* `datatype` : Datatype of each send buffer element (handle).
* `dest` : Rank of destination (integer).
* `tag` : Message tag (integer).
* `comm` : Communicator (handle).

# Output Parameters

* `request` : Communication request (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Isend` starts a standard-mode, nonblocking send. Nonblocking calls
allocate a communication `request` object and associate it with the
`request` handle (the argument `request`). The `request` can be used later to
query the status of the communication or wait for its completion.
A nonblocking send call indicates that the system may start copying data
out of the send buffer. The sender should not modify any part of the
send buffer after a nonblocking send operation is called, until the send
completes.
A send `request` can be determined being completed by calling the
`MPI_Wait`, `MPI_Waitany`, `MPI_Test`, or `MPI_Testany` with `request` returned by
this function.

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

[`MPI_Wait(3)`](./?file=MPI_Wait.md)
[`MPI_Waitany(3)`](./?file=MPI_Waitany.md)
[`MPI_Test(3)`](./?file=MPI_Test.md)
[`MPI_Testany(3)`](./?file=MPI_Testany.md)
[`MPI_Send(3)`](./?file=MPI_Send.md)
