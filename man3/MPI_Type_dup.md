# Name

`MPI_Type_dup` - Duplicates a data type with associated key values.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Type_dup(MPI_Datatype type, MPI_Datatype *newtype)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_TYPE_DUP(TYPE, NEWTYPE, IERROR)
    INTEGER    TYPE, NEWTYPE, IERROR 
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Type_dup(oldtype, newtype, ierror)
    TYPE(MPI_Datatype), INTENT(IN) :: oldtype
    TYPE(MPI_Datatype), INTENT(OUT) :: newtype
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `type` : Data type (handle).

# Output Parameters

* `newtype` : Copy of *type* (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Type_dup` is a `type` constructor that duplicates the existing `type`
with associated key values. For each key value, the respective copy
callback function determines the attribute value associated with this
key in the new communicator. One particular action that a copy callback
may take is to delete the attribute from the new data type. Returns in
`new`type`` a new data `type` with exactly the same properties as `type`, as
well as any copied cached information. The new data `type` has identical
upper bound and lower bound and yields the same net result when fully
decoded with the functions described in Section 8.6 of the MPI-2
standard. `newtype` has the same committed state as the old type.

# Notes

Note that it is not defined by the MPI standard what happens if the
attribute copy callback invokes other MPI functions. In Open MPI, it is
not valid for attribute copy callbacks (or any of their children) to add
or delete attributes on the same object on which the attribute copy
callback is being invoked.

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

[`MPI_Type_create_keyval(3)`](./?file=MPI_Type_create_keyval.md)
