# Name

`MPI_Type_create_hvector` - Creates a vector (strided) data type with
offset in bytes.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Type_create_hvector(int count, int blocklength,

    MPI_Aint stride, MPI_Datatype oldtype, MPI_Datatype *newtype)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_TYPE_CREATE_HVECTOR(COUNT, BLOCKLENGTH, STRIDE, OLDTYPE,
    NEWTYPE, IERROR)
    INTEGER    COUNT, BLOCKLENGTH, OLDTYPE, NEWTYPE, IERROR

    INTEGER(KIND=MPI_ADDRESS_KIND) STRIDE
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Type_create_hvector(count, blocklength, stride, oldtype, newtype,
        ierror)
    INTEGER, INTENT(IN) :: count, blocklength
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: stride
    TYPE(MPI_Datatype), INTENT(IN) :: oldtype
    TYPE(MPI_Datatype), INTENT(OUT) :: newtype
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `count` : Number of blocks (nonnegative integer).
* `blocklength` : Number of elements in each block (nonnegative integer).
* `stride` : Number of bytes between start of each block (integer).
* `oldtype` : Old data type (handle).

# Output Parameters

* `newtype` : New data type (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Type_create_hvector` creates a vector (strided) data type with offset
in bytes.
NOTE - This routine replaces `MPI_Type_hvector`, which is deprecated. See
the man page `MPI_Type_hvector(3`) for information about that routine.

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the STRIDE
argument only for Fortran 90. FORTRAN 77 users may use the non-portable
syntax
where `MPI_ADDRESS_KIND` is a constant defined in mpif.h and gives the
length of the declared integer in bytes.

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

[`MPI_Type_hvector(3)`](./?file=MPI_Type_hvector.md)
[`MPI_Type_vector(3)`](./?file=MPI_Type_vector.md)
