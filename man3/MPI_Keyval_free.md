# Name

`MPI_Keyval_free` - Frees attribute key for communicator cache
attribute -- use of this routine is deprecated.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Keyval_free(int *keyval)
```

## Fortran Syntax

```fortran
INCLUDE 'mpif.h'

MPI_KEYVAL_FREE(KEYVAL, IERROR)
    INTEGER    KEYVAL, IERROR
```


# Input Parameter

* `keyval` : Frees the integer key value (integer).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

Note that use of this routine is `deprecated` as of MPI-2. Please use
`MPI_Comm_free_keyval` instead.
Frees an extant attribute key. This function sets the value of `keyval` to
`MPI_KEYVAL_INVALID`. Note that it is not erroneous to free an attribute
key that is in use, because the actual free does not transpire until
after all references (in other communicators on the process) to the key
have been freed. These references need to be explicitly freed by the
program, either via calls to `MPI_Attr_delete` that free one attribute
instance, or by calls to `MPI_Comm_free` that free all attribute instances
associated with the freed communicator.

# Note

Key values are global (they can be used with any and all communicators).

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

[`MPI_Keyval_create(3)`](./?file=MPI_Keyval_create.md)
[`MPI_Comm_free_keyval(3)`](./?file=MPI_Comm_free_keyval.md)
