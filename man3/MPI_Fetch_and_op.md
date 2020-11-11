# Name

`MPI_Fetch_and_op` - Combines the contents of the origin buffer with
that of a target buffer and returns the target buffer value.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Fetch_and_op(const void *origin_addr, void *result_addr,

    MPI_Datatype datatype, int target_rank, MPI_Aint target_disp,

    MPI_Op op, MPI_Win win)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FETCH_AND_OP(ORIGIN_ADDR, RESULT_ADDR, DATATYPE, TARGET_RANK,
                 TARGET_DISP, OP, WIN, IERROR)
    <type> ORIGIN_ADDR, RESULT_ADDR(*)

    INTEGER(KIND=MPI_ADDRESS_KIND) TARGET_DISP
    INTEGER DATATYPE, TARGET_RANK, OP, WIN, IERROR 
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Fetch_and_op(origin_addr, result_addr, datatype, target_rank,
        target_disp, op, win, ierror)
    TYPE(*), DIMENSION(..), INTENT(IN) :: origin_addr
    TYPE(*), DIMENSION(..) :: result_addr
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    INTEGER, INTENT(IN) :: target_rank
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
    TYPE(MPI_Op), INTENET(IN) :: op
    TYPE(MPI_Win), INTENT(IN) :: win
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `origin_addr` : Initial address of buffer (choice).
* `result_addr` : Initial address of result buffer (choice).
* `datatype` : Data type of the entry in origin, result, and target buffers
(handle).
* `target_rank` : Rank of target (nonnegative integer).
* `target_disp` : Displacement from start of window to beginning of target buffer
(nonnegative integer).
* `op` : Reduce operation (handle).
* `win` : Window object (handle).
```


# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

Accumulate one element of type `datatype` from the origin buffer
`(origin_addr`) to the buffer at offset `target_disp`, in the target
window specified by `target_rank` and win, using the operation op
and return in the result buffer `result_addr` the contents of the target
buffer before the accumulation.
The origin and result buffers `(origin_addr` and `result_addr`) must be
disjoint. Any of the predefined operations for `MPI_Rreduce`, as well
as `MPI_NO_OP` or `MPI_REPLACE`, can be specified as op; user-defined
functions cannot be used. The `datatype` argument must be a predefined
datatype. The operation is executed atomically.
A new predefined operation, `MPI_REPLACE`, is defined. It corresponds to
the associative function f(a, b) =b; that is, the current value in the
target memory is replaced by the value supplied by the origin.
A new predefined operation, `MPI_NO_OP`, is defined. It corresponds to the
assiciative function f(a, b) = a; that is the current value in the
target memory is returned in the result buffer at the origin and no
operation is performed on the target buffer.

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the
`TARGET_DISP` argument only for Fortran 90. FORTRAN 77 users may use the
non-portable syntax
where `MPI_ADDRESS_KIND` is a constant defined in mpif.h and gives the
length of the declared integer in bytes.

# Notes

It is the user's responsibility to guarantee that, when using the
accumulate functions, the target displacement argument is such that
accesses to the window are properly aligned according to the data type
arguments in the call to the `MPI_Fetch_and_op` function.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler
`MPI_ERRORS_RETURN` may be used to cause error values to be returned. Note
that MPI does not guarantee that an MPI program can continue past an
error.

# See Also

[`MPI_Get_accumulate(3)`](./?file=MPI_Get_accumulate.md)
[`MPI_Reduce(3)`](./?file=MPI_Reduce.md)
