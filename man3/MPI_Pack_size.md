# Name

`MPI_Pack_size` - Returns the upper bound on the amount of space
needed to pack a message.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Pack_size(int incount, MPI_Datatype datatype, MPI_Comm comm,
    int *size)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_PACK_SIZE(INCOUNT, DATATYPE, COMM, SIZE, IERROR)
    INTEGER    INCOUNT, DATATYPE, COMM, SIZE, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Pack_size(incount, datatype, comm, size, ierror)
    INTEGER, INTENT(IN) :: incount
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, INTENT(OUT) :: size
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `incount` : Count argument to packing call (integer).
* `datatype` : Datatype argument to packing call (handle).
* `comm` : Communicator argument to packing call (handle).

# Output Parameters

* `size` : Upper bound on size of packed message, in bytes (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Pack_size` allows the application to find out how much space is
needed to pack a message. A call to `MPI_Pack_size(incount`, datatype,
comm, `size`) returns in `size` an upper bound on the increment in position
that would occur in a call to `MPI_Pack`, with the same values for
incount, datatype, and comm.
Rationale: The call returns an upper bound, rather than an exact
bound, since the exact amount of space needed to pack the message may
depend on the context (e.g., first message packed in a packing unit may
take more space).

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

[`MPI_Pack(3)`](./?file=MPI_Pack.md)
[`MPI_Unpack(3)`](./?file=MPI_Unpack.md)
