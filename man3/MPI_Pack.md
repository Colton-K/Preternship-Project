# Name

`MPI_Pack` - Packs data of a given datatype into contiguous memory.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Pack(const void *inbuf, int incount, MPI_Datatype datatype,

    void *outbuf, int outsize, int *position, MPI_Comm comm)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_PACK(INBUF, INCOUNT, DATATYPE, OUTBUF,OUTSIZE, POSITION,
        COMM, IERROR)
    <type>    INBUF(*), OUTBUF(*)
    INTEGER    INCOUNT, DATATYPE, OUTSIZE, POSITION, COMM, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Pack(inbuf, incount, datatype, outbuf, outsize, position, comm, ierror)
    TYPE(*), DIMENSION(..), INTENT(IN) :: inbuf
    TYPE(*), DIMENSION(..) :: outbuf
    INTEGER, INTENT(IN) :: incount, outsize
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    INTEGER, INTENT(INOUT) :: position
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `inbuf` : Input buffer start (choice).
* `incount` : Number of input data items (integer).
* `datatype` : Datatype of each input data item (handle).
* `outsize` : Output buffer size, in bytes (integer).
* `comm` : Communicator for packed message (handle).

# Input/Output Parameter

* `position` : Current position in buffer, in bytes (integer).

# Output Parameters

* `outbuf` : Output buffer start (choice).
* `IERROR` : Fortran only: Error status (integer).

# Description

Packs the message in the send buffer specified by inbuf, incount,
`datatype` into the buffer space specified by `outbuf` and outsize.
The input buffer can be any communication buffer allowed in `MPI_Send.`
The output buffer is a contiguous storage area containing outsize
bytes, starting at the address `outbuf` (length is counted in bytes, not
elements, as if it were a communication buffer for a message of type
`MPI_Packed).`
The input value of `position` is the first location in the output buffer
to be used for packing. `position` is incremented by the size of the
packed message, and the output value of `position` is the first location
in the output buffer following the locations occupied by the packed
message. The `comm` argument is the communicator that will be
subsequently used for sending the packed message.
Example: An example using `MPI_Pack:`
        int position, i, j, a[2];
        char buff[1000];
        MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
        if (myrank == 0)
        `position` = 0;
          MPI_Pack(&i, 1, MPI_INT, buff, 1000, &position, MPI_COMM_WORLD);
          MPI_Pack(&j, 1, MPI_INT, buff, 1000, &position, MPI_COMM_WORLD);
          MPI_Send( buff, position, MPI_PACKED, 1, 0, MPI_COMM_WORLD);
        else  / RECEIVER CODE /
          MPI_Recv( a, 2, MPI_INT, 0, 0, MPI_COMM_WORLD)

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

[`MPI_Unpack(3)`](./?file=MPI_Unpack.md)
[`MPI_Pack_size(3)`](./?file=MPI_Pack_size.md)
