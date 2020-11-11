# Name

`MPI_Comm_create_keyval` - Generates a new attribute key.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Comm_create_keyval(MPI_Comm_copy_attr_function

    *comm_copy_attr_fn, MPI_Comm_delete_attr_function
    *comm_delete_attr_fn, int *comm_keyval,
    void *extra_state)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_CREATE_KEYVAL(COMM_COPY_ATTR_FN, COMM_DELETE_ATTR_FN,
    COMM_KEYVAL, EXTRA_STATE, IERROR)
    EXTERNAL COMM_COPY_ATTR_FN, COMM_DELETE_ATTR_FN
    INTEGER COMM_KEYVAL, IERROR 

    INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Comm_create_keyval(comm_copy_attr_fn, comm_delete_attr_fn, comm_keyval,
        extra_state, ierror)
    PROCEDURE(MPI_Comm_copy_attr_function) :: comm_copy_attr_fn
    PROCEDURE(MPI_Comm_delete_attr_function) :: comm_delete_attr_fn
    INTEGER, INTENT(OUT) :: comm_keyval
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `comm_copy_attr_fn` : Copy callback function for *comm_keyval* (function).
* `comm_delete_attr_fn` : Delete callback function for *comm_keyval* (function).
* `extra_state` : Extra state for callback functions.

# Output Parameter

* `comm_keyval` : Key value for future access (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

This function replaces `MPI_Keyval_create`, the use of which is
deprecated. The C binding is identical. The Fortran binding differs in
that `extra_state` is an address-sized integer. Also, the copy and
delete callback functions have Fortran bindings that are consistent with
address-sized attributes.
The argument `comm_copy_attr_fn` may be specified as
`MPI_COMM_NULL_COPY_FN` or `MPI_COMM_DUP_FN` from C or Fortran.
`MPI_COMM_NULL_COPY_FN` is a function that does nothing more than
returning `flag` = 0 and `MPI_SUCCESS`. `MPI_COMM_DUP_FN` is a simple-minded
copy function that sets `flag` = 1, returns the value of
`attribute_val_in` in `attribute_val_out`, and returns `MPI_SUCCESS.`
These replace the MPI-1 predefined callbacks `MPI_NULL_COPY_FN` and
`MPI_DUP_FN`, the use of which is deprecated.
The C callback functions are:
    typedef int MPI_Comm_copy_attr_function(MPI_Comm oldcomm, int comm_keyval,
                 void extra_state, void attribute_val_in,
                 void attribute_val_out, int flag);
and
    typedef int MPI_Comm_delete_attr_function(MPI_Comm comm, int comm_keyval,
                 void attribute_val, void extra_state);
which are the same as the MPI-1.1 calls but with a new name. The old
names are deprecated.
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
See the MPI man page for a full list of MPI error codes.
SEE ALSO

# See Also
