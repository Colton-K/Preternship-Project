# Name

`MPI_Type_create_indexed_block`, `MPI_Type_create_hindexed_block` -
Creates an indexed data type with the same block length for all blocks.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Type_create_indexed_block(int count, int blocklength, const int array_of_displacements[], MPI_Datatype oldtype, MPI_Datatype *newtype)

int MPI_Type_create_hindexed_block(int count, int blocklength, const MPI_Aint array_of_displacements[], MPI_Datatype oldtype, MPI_Datatype *newtype)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_TYPE_CREATE_INDEXED_BLOCK(COUNT, BLOCKLENGTH,
        ARRAY_OF_DISPLACEMENTS, OLDTYPE, NEWTYPE, IERROR)
    INTEGER    COUNT, BLOCKLENGTH, ARRAY_OF_DISPLACEMENTS(*),
            OLDTYPE, NEWTYPE, IERROR 

MPI_TYPE_CREATE_HINDEXED_BLOCK(COUNT, BLOCKLENGTH,
        ARRAY_OF_DISPLACEMENTS, OLDTYPE, NEWTYPE, IERROR)
    INTEGER    COUNT, BLOCKLENGTH, OLDTYPE, NEWTYPE

    INTEGER(KIND=MPI_ADDRESS_KIND) ARRAY_OF_DISPLACEMENTS(*)
    INTEGER    IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Type_create_indexed_block(count, blocklength, array_of_displacements,
        oldtype, newtype, ierror)
    INTEGER, INTENT(IN) :: count, blocklength,
    array_of_displacements(count)
    TYPE(MPI_Datatype), INTENT(IN) :: oldtype
    TYPE(MPI_Datatype), INTENT(OUT) :: newtype
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Type_create_hindexed_block(count, blocklength, array_of_displacements,
        oldtype, newtype, ierror)
    INTEGER, INTENT(IN) :: count, blocklength
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) ::
    array_of_displacements(count)
    TYPE(MPI_Datatype), INTENT(IN) :: oldtype
    TYPE(MPI_Datatype), INTENT(OUT) :: newtype
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `count` : Length of array of displacements (integer).
* `blocklength` : Size of block (integer).
* `array_of_displacements` : Array of displacements (array of integers). In units of the extent

of *oldtype* for MPI_Type_create_indexed_block and bytes for

MPI_Type_create_hindexed_block.
* `oldtype` : Old data type (handle).
```


# Output Parameters

* `newtype` : New data type (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Type_create_indexed_block` and `MPI_Type_create_hindexed_block` create
an indexed data type with the same block length for all blocks. The only
difference between the two functions is `MPI_Type_create_indexed_block`
takes an array of displacements in units of the extent of oldtype
while `MPI_Type_create_hindexed_block` takes displacements in bytes.

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

[`MPI_Type_indexed(3)`](./?file=MPI_Type_indexed.md)
