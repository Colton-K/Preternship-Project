# Name

`MPI_Imrecv` - Non-blocking receive for a matched message

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Imrecv(void *buf, int count, MPI_Datatype type,

    MPI_Message *message, MPI_Request *request)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_IMRECV(BUF, COUNT, DATATYPE, MESSAGE, REQUEST, IERROR)
    <type>    BUF(*)
    INTEGER    COUNT, DATATYPE, MESSAGE, REQUEST, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Imrecv(buf, count, datatype, message, request, ierror)
    TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
    INTEGER, INTENT(IN) :: count
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    TYPE(MPI_Message), INTENT(INOUT) :: message
    TYPE(MPI_Request), INTENT(OUT) :: request
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `count` : Number of elements to receive (nonnegative integer).
* `datatype` : Datatype of each send buffer element (handle).
* `message` : Message (handle).

# Output Parameters

* `buf` : Initial address of receive buffer (choice).
* `request` : Request (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

The functions `MPI_Mrecv` and `MPI_Imrecv` receive messages that have been
previously matched by a matching probe.
The `request` returned from `MPI_Imrecv` can be used with any of the
`MPI_Test` and `MPI_Wait` variants, like any non-blocking receive request.
If `MPI_Imrecv` is called with `MPI_MESSAGE_NULL` as the `message` argument, a
call to one of the `MPI_Test` or `MPI_Wait` variants will return immediately
with the `status` object set to `source` = `MPI_PROC_NULL`, `tag` =
`MPI_ANY_TAG`, and `count` = 0, as if a receive from `MPI_PROC_NULL` was
issued.
If reception of a matched `message` is started with `MPI_Imrecv`, then it is
possible to cancel the returned `request` with `MPI_Cancel`. If `MPI_Cancel`
succeeds, the matched `message` must be found by a subsequent `message`
probe `(MPI_Probe`, `MPI_Iprobe`, `MPI_Mprobe`, or `MPI_Improbe)`, received by a
subsequent receive operation or canceled by the sender.
Note, however, that is it possible for the cancellation of operations
initiated with `MPI_Imrecv` to fail. An example of a failing case is when
canceling the matched `message` receive would violate MPI `message` ordering
rules (e.g., if another `message` matching the same `message` signature has
matched -- and possible received -- before this `MPI_Imrecv` is
canceled).

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

[`MPI_Mprobe(3)`](./?file=MPI_Mprobe.md)
[`MPI_Improbe(3)`](./?file=MPI_Improbe.md)
[`MPI_Probe(3)`](./?file=MPI_Probe.md)
[`MPI_Iprobe(3)`](./?file=MPI_Iprobe.md)
[`MPI_Imrecv(3)`](./?file=MPI_Imrecv.md)
[`MPI_Cancel(3)`](./?file=MPI_Cancel.md)
