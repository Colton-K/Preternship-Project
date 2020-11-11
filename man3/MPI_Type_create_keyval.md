# Name

`MPI_Type_create_keyval` - Generates a new attribute key for caching
on data types.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Type_create_keyval(MPI_Type_copy_attr_function *type_copy_attr_fn,

    MPI_Type_delete_attr_function *type_delete_attr_fn,
    int *type_keyval, void *extra_state)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_TYPE_CREATE_KEYVAL(TYPE_COPY_ATTR_FN, TYPE_DELETE_ATTR_FN,
        TYPE_KEYVAL, EXTRA_STATE, IERROR)
    EXTERNAL TYPE_COPY_ATTR_FN, TYPE_DELETE_ATTR_FN
    INTEGER    TYPE_KEYVAL, IERROR 

    INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Type_create_keyval(type_copy_attr_fn, type_delete_attr_fn, type_keyval,
        extra_state, ierror)
    PROCEDURE(MPI_Type_copy_attr_function) :: type_copy_attr_fn
    PROCEDURE(MPI_Type_delete_attr_function) :: type_delete_attr_fn
    INTEGER, INTENT(OUT) :: type_keyval
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `type_copy_attr_fn` : Copy callback function for *type_keyval* (function).
* `type_delete_attr_fn` : Delete callback function for *type_keyval* (function).
* `extra_state` : Extra state for callback functions.

# Output Parameters

* `type_keyval` : Key value for future access (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Type_create_keyval` generates a new attribute key for caching on data
types. This routine partially replaces `MPI_Keyval_create.`
The argument `type_copy_attr_fn` may be specified as
`MPI_TYPE_NULL_COPY_FN` or `MPI_TYPE_DUP_FN` from C or Fortran.
`MPI_TYPE_NULL_COPY_FN` is a function that does nothing other than
returning `flag` = 0 and `MPI_SUCCESS`. `MPI_TYPE_DUP_FN` is a simple-minded
copy function that sets `flag` = 1, returns the value of
`attribute_val_in` in `attribute_val_out`, and returns `MPI_SUCCESS.`
The argument `type_delete_attr_fn` may be specified as
`MPI_TYPE_NULL_DELETE_FN` from C or Fortran. `MPI_TYPE_NULL_DELETE_FN` is a
function that does nothing beyond returning `MPI_SUCCESS`. The C callback
functions are:
    typedef int MPI_Type_copy_attr_function(MPI_Datatype oldtype,
                int type_keyval, void extra_state, void attribute_val_in,
                void attribute_val_out, int flag);
and
    typedef int MPI_Type_delete_attr_function(MPI_Datatype type, int type_keyval,
                 void attribute_val, void extra_state);
The Fortran callback functions are:
and

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the
`EXTRA_STATE` argument only for Fortran 90. FORTRAN 77 users may use the
non-portable syntax
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

[`MPI_Type_free_keyval(3)`](./?file=MPI_Type_free_keyval.md)
