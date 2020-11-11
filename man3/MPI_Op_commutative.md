# Name

`MPI_Op_commutative` - Query of commutativity of reduction operation.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Op_commutative(MPI_Op op, int *commute)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_OP_COMMUTATIVE(OP, COMMUTE, IERROR)
    LOGICAL    COMMUTE
    INTEGER    OP, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Op_commutative(op, commute, ierror)
    TYPE(MPI_Op), INTENT(IN) :: op
    INTEGER, INTENT(OUT) :: commute
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `op` : Operation (handle).

# Output Parameters

* `commute` : True if op is commutative, false otherwise (logical).
* `IERROR` : Fortran only: Error status (integer).

# Description

Reduction operations can be queried for their commutativity.

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

[`MPI_Op_create(3)`](./?file=MPI_Op_create.md)
