# Name

`MPI_Unpack_external` - Reads data from a portable format

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Unpack_external(const char datarep[], const void *inbuf,

    MPI_Aint insize, MPI_Aint *position,
    void *outbuf, int outcount,

    MPI_Datatype datatype)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_UNPACK_EXTERNAL(DATAREP, INBUF, INSIZE, POSITION,
    OUTBUF, OUTCOUNT, DATATYPE, IERROR)
    INTEGER        OUTCOUNT, DATATYPE, IERROR

    INTEGER(KIND=MPI_ADDRESS_KIND) INSIZE, POSITION
    CHARACTER*(*)    DATAREP
    <type>        INBUF(*), OUTBUF(*)
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Unpack_external(datarep, inbuf, insize, position, outbuf, outcount,
        datatype, ierror)
    CHARACTER(LEN=*), INTENT(IN) :: datarep
    TYPE(*), DIMENSION(..), INTENT(IN) :: inbuf
    TYPE(*), DIMENSION(..) :: outbuf
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: insize
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(INOUT) :: position
    INTEGER, INTENT(IN) :: outcount
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `datarep` : Data Representation (string).
* `inbuf` : Input buffer start (choice).
* `insize` : Size of input buffer, in bytes (integer).
* `outcount` : Number of items to be unpacked (integer).
* `datatype` : Datatype of each output data item (handle).

# Input/Output Parameter

* `position` : Current position in buffer, in bytes (integer).

# Output Parameters

* `outbuf` : Output buffer start (choice).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Unpack_external` unpacks data from the external32 format, a universal
data representation defined by the MPI Forum. This format is useful for
exchanging data between MPI implementations, or when writing data to a
file.
The input buffer is a contiguous storage area pointed to by inbuf
containing `insize` bytes. The output buffer can be any communication
buffer allowed in `MPI_Recv`, and is specified by outbuf, outcount,
and datatype.
The input value of ``position`` is the first `position` in `inbuf` to be
read for unpacking (measured in bytes, not elements, relative to the
start of the buffer). When the function returns, `position` is
incremented by the size of the packed message, so that it points to the
first location in `inbuf` following the message that was unpacked. This
way it may be used as input to a subsequent call to `MPI_Unpack_external.`

# Notes

Note the difference between `MPI_Recv` and `MPI_Unpack_external`: In
`MPI_Recv`, the `count` argument specifies the maximum number of items
that can be received. In `MPI_Unpack_external`, the `outcount` argument
specifies the actual number of items that are to be unpacked. With a
regular receive operation, the incoming message size determines the
number of components that will be received. With `MPI_Unpack_external`, it
is up to the user to specify how many components to unpack, since the
user may wish to unpack the received message multiple times into various
buffers.
To understand the behavior of pack and unpack, it is convenient to think
of the data part of a message as being the sequence obtained by
concatenating the successive values sent in that message. The pack
operation stores this sequence in the buffer space, as if sending the
message to that buffer. The unpack operation retrieves this sequence
from buffer space, as if receiving a message from that buffer. (It is
helpful to think of internal Fortran files or sscanf in C for a similar
function.)
Several messages can be successively packed into one packing unit. This
is effected by several successive related calls to `MPI_Pack_external,`
where the first call provides position=0, and each successive call
inputs the value of `position` that was output by the previous call,
along with the same values for `outbuf` and outcount. This packing
unit now contains the equivalent information that would have been stored
in a message by one send call with a send buffer that is the
"concatenation" of the individual send buffers.
A packing unit can be sent using type `MPI_BYTE`. Any point-to-point or
collective communication function can be used to move the sequence of
bytes that forms the packing unit from one process to another. This
packing unit can now be received using any receive operation, with any
datatype: The type-matching rules are relaxed for messages sent with
type `MPI_BYTE.`
A packing unit can be unpacked into several successive messages. This is
effected by several successive related calls to `MPI_Unpack_external,`
where the first call provides position=0, and each successive call
inputs the value of `position` that was output by the previous call, and
the same values for `inbuf` and insize.
The concatenation of two packing units is not necessarily a packing
unit; nor is a substring of a packing unit necessarily a packing unit.
Thus, one cannot concatenate two packing units and then unpack the
result as one packing unit; nor can one unpack a substring of a packing
unit as a separate packing unit. Each packing unit that was created by a
related sequence of pack calls must be unpacked as a unit by a sequence
of related unpack calls.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
See the MPI man page for a full list of MPI error codes.

# See Also

[`MPI_Pack_external(3)`](./?file=MPI_Pack_external.md)
[`MPI_Pack_external_size(3)`](./?file=MPI_Pack_external_size.md)
[`MPI_Recv(3)`](./?file=MPI_Recv.md)
    sscanf(3C)
