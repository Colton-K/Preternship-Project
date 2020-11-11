# Name

`MPI_Attr_get` - Retrieves attribute value by key -- use of this
routine is deprecated.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Attr_get(MPI_Comm comm, int keyval,void *attribute_val,
    int *flag )
```

## Fortran Syntax

```fortran
INCLUDE 'mpif.h'

MPI_ATTR_GET(COMM, KEYVAL, ATTRIBUTE_VAL, FLAG, IERROR)
    INTEGER    COMM, KEYVAL, ATTRIBUTE_VAL, IERROR
    LOGICAL    FLAG
```


# Input Parameters

* `comm` : Communicator to which attribute is attached (handle).
* `keyval` :  Key value (integer).
 Key value (integer).
```


# Output Parameters

* `attribute_val` : Attribute value, unless flag = false.
* `flag` : True if an attribute value was extracted; false if no attribute is
associated with the key.
* `IERROR` : Fortran only: Error status (integer).
```


# Description

Note that use of this routine is `deprecated` as of MPI-2, and was
`deleted` in MPI-3. Please use `MPI_Comm_get_attr`. This function does not
have a `mpi_f08` binding.
Retrieves attribute value by key. The call is erroneous if there is no
key with value keyval. On the other hand, the call is correct if the key
value exists, but no attribute is attached on `comm` for that key; in such
case, the call returns `flag` = false. In particular `MPI_KEYVAL_INVALID` is
an erroneous key value.

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

[`MPI_Comm_get_attr(3)`](./?file=MPI_Comm_get_attr.md)
