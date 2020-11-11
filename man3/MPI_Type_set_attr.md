# Name

`MPI_Type_set_attr` - Sets a key value/attribute pair to a data type.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Type_set_attr(MPI_Datatype type, int type_keyval,
    void *attribute_val)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_TYPE_SET_ATTR(TYPE, TYPE_KEYVAL, ATTRIBUTE_VAL, IERROR)
    INTEGER    TYPE, TYPE_KEYVAL, IERROR

    INTEGER(KIND=MPI_ADDRESS_KIND) ATTRIBUTE_VAL
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Type_set_attr(datatype, type_keyval, attribute_val, ierror)
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    INTEGER, INTENT(IN) :: type_keyval
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `type` : Data type to which attribute will be attached (handle).

# Input Parameters

* `type_keyval` : Key value (integer).
* `attribute_val` : Attribute value.

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

For the given data type, `MPI_Type_set_attr` sets the key value to the
value of the specified attribute.

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the
`ATTRIBUTE_VAL` argument only for Fortran 90. FORTRAN 77 users may use
the non-portable syntax
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

[`MPI_Type_get_attr(3)`](./?file=MPI_Type_get_attr.md)
