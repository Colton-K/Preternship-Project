# Name

`MPI_Type_get_attr` - Returns the attribute associated with a data
type.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Type_get_attr(MPI_Datatype type, int type_keyval, void *attribute_val, int *flag)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_TYPE_GET_ATTR(TYPE, TYPE_KEYVAL, ATTRIBUTE_VAL, FLAG, IERROR)
    INTEGER    TYPE, TYPE_KEYVAL, IERROR 

    INTEGER(KIND=MPI_ADDRESS_KIND) ATTRIBUTE_VAL
    LOGICAL FLAG
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Type_get_attr(datatype, type_keyval, attribute_val, flag, ierror)
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    INTEGER, INTENT(IN) :: type_keyval
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
    LOGICAL, INTENT(OUT) :: flag
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `type` : Data type to which the attribute is attached (handle).
* `type_keyval` : Key value (integer).

# Output Parameters

* `attribute_val` : Attribute value, unless *flag* = false
* `flag` : "false" if no attribute is associated with the key (logical).
* `IERROR` : Fortran only: Error status (integer).

# Description

For the given data type, `MPI_Type_get_attr` returns an attribute value
that corresponds to the specified key value.

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the
`ATTRIBUTE_VAL` argument only for Fortran 90. Sun FORTRAN 77 users may
use the non-portable syntax
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

[`MPI_Type_set_attr(3)`](./?file=MPI_Type_set_attr.md)
