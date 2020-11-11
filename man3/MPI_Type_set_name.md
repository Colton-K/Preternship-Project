# Name

`MPI_Type_set_name` - Sets the name of a data type.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Type_set_name(MPI_Datatype type, const char *type_name)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_TYPE_SET_NAME(TYPE, TYPE_NAME, IERROR)
    INTEGER    TYPE, IERROR
    CHARACTER*(*) TYPE_NAME
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Type_set_name(datatype, type_name, ierror)
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    CHARACTER(LEN=*), INTENT(IN) :: type_name
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `type` : Data type for which the identifier is to be set (handle).

# Input Parameter

* `type_name` : The character string remembered as the name (string).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Type_set_name` associates a printable identifier with an MPI data
type.

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

[`MPI_Type_get_name(3)`](./?file=MPI_Type_get_name.md)
