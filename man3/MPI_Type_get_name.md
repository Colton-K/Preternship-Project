# Name

`MPI_Type_get_name` - Gets the name of a data type.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Type_get_name(MPI_Datatype type, char *type_name,
    int *resultlen)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_TYPE_GET_NAME(TYPE, TYPE_NAME, RESULTLEN, IERROR)
    INTEGER    TYPE, RESULTLEN, IERROR 
    CHARACTER*(*) TYPE_NAME
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Type_get_name(datatype, type_name, resultlen, ierror)
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    CHARACTER(LEN=MPI_MAX_OBJECT_NAME), INTENT(OUT) :: type_name
    INTEGER, INTENT(OUT) :: resultlen
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `type` : Data type whose name is to be returned (handle).

# Output Parameters

* `type_name` : The name previously stored on the data type, or an empty string if
not such name exists (string).
* `resultlen` : Length of returned name (integer).
* `IERROR` : Fortran only: Error status (integer).
```


# Description

`MPI_Type_get_name` returns the printable identifier associated with an
MPI data type.

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

[`MPI_Type_set_name(3)`](./?file=MPI_Type_set_name.md)
