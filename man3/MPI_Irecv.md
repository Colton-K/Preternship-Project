# Name

`MPI_Irecv` - Starts a standard-mode, nonblocking receive.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Irecv(void *buf, int count, MPI_Datatype datatype,

        int source, int tag, MPI_Comm comm, MPI_Request *request)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_IRECV(BUF, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST,
        IERROR)
    <type>    BUF(*)
    INTEGER    COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Irecv(buf, count, datatype, source, tag, comm, request, ierror)
    TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
    INTEGER, INTENT(IN) :: count, source, tag
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Request), INTENT(OUT) :: request
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `buf` : Initial address of receive buffer (choice).
* `count` : Number of elements in receive buffer (integer).
* `datatype` : Datatype of each receive buffer element (handle).
* `source` : Rank of source (integer).
* `tag` : Message tag (integer).
* `comm` : Communicator (handle).

# Output Parameters

* `request` : Communication request (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

Nonblocking calls allocate a communication `request` object and associate
it with the `request` handle (the argument `request`). The `request` can be
used later to query the status of the communication or wait for its
completion.
A nonblocking receive call indicates that the system may start writing
data into the receive buffer. The receiver should not access any part of
the receive buffer after a nonblocking receive operation is called,
until the receive completes.
A receive `request` can be determined being completed by calling the
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

[`MPI_Probe(3)`](./?file=MPI_Probe.md)
[`MPI_Test(3)`](./?file=MPI_Test.md)
[`MPI_Testany(3)`](./?file=MPI_Testany.md)
[`MPI_Wait(3)`](./?file=MPI_Wait.md)
[`MPI_Waitany(3)`](./?file=MPI_Waitany.md)
[`MPI_Recv(3)`](./?file=MPI_Recv.md)
