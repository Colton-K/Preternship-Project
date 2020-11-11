# Name

`MPI_Type_delete_attr` - Deletes a datatype-caching attribute value
associated with a key.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Type_delete_attr(MPI_Datatype type, int type_keyval)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_TYPE_DELETE_ATTR(TYPE, TYPE_KEYVAL, IERROR)
    INTEGER    TYPE, TYPE_KEYVAL, IERROR 
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Type_delete_attr(datatype, type_keyval, ierror)
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    INTEGER, INTENT(IN) :: type_keyval
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `type` : Data type from which the attribute is deleted (handle).n

# Input Parameter

* `type_keyval` : Key value (integer).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Type_delete_attr` deletes a datatype-caching attribute value
associated with a key. This routines partially replaces `MPI_Attr_delete,`
which is now deprecated.

# Notes

Note that it is not defined by the MPI standard what happens if the
`delete_fn` callback invokes other MPI functions. In Open MPI, it is not
valid for `delete_fn` callbacks (or any of their children) to add or
delete attributes on the same object on which the `delete_fn` callback is
being invoked.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
