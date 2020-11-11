# Name

`MPI_Mprobe` - Blocking matched probe for a message.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Mprobe(int source, int tag, MPI_Comm comm,

    MPI_Message *message, MPI_Status *status)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_MPROBE(SOURCE, TAG, COMM, MESSAGE, STATUS, IERROR)
    INTEGER    SOURCE, TAG, COMM, MESSAGE

    INTEGER    STATUS(MPI_STATUS_SIZE), IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Mprobe(source, tag, comm, message, status, ierror)
    INTEGER, INTENT(IN) :: source, tag
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Message), INTENT(OUT) :: message
    TYPE(MPI_Status) :: status
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `source` : Source rank or MPI_ANY_SOURCE (integer).
* `tag` : Tag value or MPI_ANY_TAG (integer).
* `comm` : Communicator (handle).

# Output Parameters

* `message` : Message (handle).
* `status` : Status object (status).
* `IERROR` : Fortran only: Error status (integer).

# Description

Like `MPI_Probe` and `MPI_Iprobe`, the `MPI_Mprobe` and `MPI_Improbe` operations
allow incoming messages to be queried without actually receiving them,
except that `MPI_Mprobe` and `MPI_Improbe` provide a mechanism to receive
the specific `message` that was matched regardless of other intervening
probe or receive operations. This gives the application an opportunity
to decide how to receive the message, based on the information returned
by the probe. In particular, the application may allocate memory for the
receive buffer according to the length of the probed message.
A matching probe with `MPI_PROC_NULL` as `source` returns `message` =
`MPI_MESSAGE_NO_PROC`, and the `status` object returns `source` =
`MPI_PROC_NULL`, `tag` = `MPI_ANY_TAG`, and count = 0.
When `MPI_Mprobe` returns (from a `non-MPI_PROC_NULL` source), the matched
message can then be received by passing the `message` handle to the
`MPI_Mrecv` or `MPI_Imrecv` functions.

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

[`MPI_Improbe(3)`](./?file=MPI_Improbe.md)
[`MPI_Probe(3)`](./?file=MPI_Probe.md)
[`MPI_Iprobe(3)`](./?file=MPI_Iprobe.md)
[`MPI_Mrecv(3)`](./?file=MPI_Mrecv.md)
[`MPI_Imrecv(3)`](./?file=MPI_Imrecv.md)
[`MPI_Cancel(3)`](./?file=MPI_Cancel.md)
