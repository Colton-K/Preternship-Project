# Name

`MPI_Send` - Performs a standard-mode blocking send.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Send(const void *buf, int count, MPI_Datatype datatype, int dest,

    int tag, MPI_Comm comm)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_SEND(BUF, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)
    <type>    BUF(*)
    INTEGER    COUNT, DATATYPE, DEST, TAG, COMM, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Send(buf, count, datatype, dest, tag, comm, ierror)
    TYPE(*), DIMENSION(..), INTENT(IN) :: buf
    INTEGER, INTENT(IN) :: count, dest, tag
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `buf` : Initial address of send buffer (choice).
* `count` : Number of elements send (nonnegative integer).
* `datatype` : Datatype of each send buffer element (handle).
* `dest` : Rank of destination (integer).
* `tag` : Message tag (integer).
* `comm` : Communicator (handle).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Send` performs a standard-mode, blocking send.

# Note

This routine will block until the message is sent to the destination.
For an in-depth explanation of the semantics of the standard-mode send,
refer to the MPI-1 Standard.

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

[`MPI_Isend(3)`](./?file=MPI_Isend.md)
[`MPI_Bsend(3)`](./?file=MPI_Bsend.md)
[`MPI_Recv(3)`](./?file=MPI_Recv.md)
