# Name

`MPI_Mrecv` - Blocking receive for a matched message

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Mrecv(void *buf, int count, MPI_Datatype type,

    MPI_Message *message, MPI_Status *status)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_MRECV(BUF, COUNT, DATATYPE, MESSAGE, STATUS, IERROR)
    <type>    BUF(*)
    INTEGER    COUNT, DATATYPE, MESSAGE

    INTEGER    STATUS(MPI_STATUS_SIZE), IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Mrecv(buf, count, datatype, message, status, ierror)
    TYPE(*), DIMENSION(..) :: buf
    INTEGER, INTENT(IN) :: count
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    TYPE(MPI_Message), INTENT(INOUT) :: message
    TYPE(MPI_Status) :: status
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `count` : Number of elements to receive (nonnegative integer).
* `datatype` : Datatype of each send buffer element (handle).
* `message` : Message (handle).

# Output Parameters

* `buf` : Initial address of receive buffer (choice).
* `status` : Status object (status).
* `IERROR` : Fortran only: Error status (integer).

# Description

The functions `MPI_Mrecv` and `MPI_Imrecv` receive messages that have been
previously matched by a matching probe.
If `MPI_Mrecv` is called with `MPI_MESSAGE_NULL` as the `message` argument,
the call returns immediately with the `status` object set to `source` =
`MPI_PROC_NULL`, `tag` = `MPI_ANY_TAG`, and `count` = 0, as if a receive
from `MPI_PROC_NULL` was issued.

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
