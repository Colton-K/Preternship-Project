# Name

`MPI_Pack_external_size` - Calculates upper bound on space needed to
write to a portable format

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Pack_external_size(char *datarep, int incount,

    MPI_Datatype datatype, MPI_Aint *size)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_PACK_EXTERNAL_SIZE(DATAREP, INCOUNT, DATATYPE, SIZE, IERROR)
    INTEGER        INCOUNT, DATATYPE, IERROR

    INTEGER(KIND=MPI_ADDRESS_KIND) SIZE
    CHARACTER*(*)    DATAREP
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Pack_external_size(datarep, incount, datatype, size, ierror)
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    INTEGER, INTENT(IN) :: incount
    CHARACTER(LEN=*), INTENT(IN) :: datarep
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: size
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `datarep` : Data representation (string).
* `incount` : Number of input data items (integer).
* `datatype` : Datatype of each input data item (handle).

# Output Parameters

* `size` : Upper bound on size of packed message, in bytes (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Pack_external_size` allows the application to find out how much space
is needed to pack a message in the portable format defined by the MPI
Forum. It returns in `size` an upper bound on the increment in
`position` that would occur in a call to `MPI_Pack_external` with the same
values for datarep, incount, and datatype.
The call returns an upper bound, rather than an exact bound, as the
exact amount of space needed to pack the message may depend on context
and alignment (e.g., the first message packed in a packing unit may take
more space).

# Notes

The `datarep` argument specifies the data format. The only valid value
in the current version of MPI is "external32". The argument is
provided for future extensibility.

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
[`MPI_Unpack_external(3)`](./?file=MPI_Unpack_external.md)
