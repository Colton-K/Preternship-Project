# Name

`MPI_Win_create` - One-sided MPI call that returns a window object for
RMA operations.

# Syntax

## C Syntax

```c
#include <mpi.h>

MPI_Win_create(void *base, MPI_Aint size, int disp_unit,

    MPI_Info info, MPI_Comm comm, MPI_Win *win)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_CREATE(BASE, SIZE, DISP_UNIT, INFO, COMM, WIN, IERROR)
    <type> BASE(*)

    INTEGER(KIND=MPI_ADDRESS_KIND) SIZE
    INTEGER DISP_UNIT, INFO, COMM, WIN, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_create(base, size, disp_unit, info, comm, win, ierror)
    TYPE(*), DIMENSION(..), ASYNCHRONOUS :: base
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: size
    INTEGER, INTENT(IN) :: disp_unit
    TYPE(MPI_Info), INTENT(IN) :: info
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Win), INTENT(OUT) :: win
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `base` : Initial address of window (choice).
* `size` : Size of window in bytes (nonnegative integer).
* `disp_unit` : Local unit size for displacements, in bytes (positive integer).
* `info` : Info argument (handle).
* `comm` : Communicator (handle).

# Output Parameters

* `win` : Window object returned by the call (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_create` is a one-sided MPI communication collective call executed
by all processes in the group of comm. It returns a window object that
can be used by these processes to perform RMA operations. Each process
specifies a window of existing memory that it exposes to RMA accesses by
the processes in the group of comm. The window consists of size
bytes, starting at address base. A process may elect to expose no
memory by specifying `size` = 0.
If the `base` value used by `MPI_Win_create` was allocated by
`MPI_Alloc_mem`, the `size` of the window can be no larger than the value
set by the `MPI_ALLOC_MEM` function.
The displacement unit argument is provided to facilitate address
arithmetic in RMA operations: the target displacement argument of an RMA
operation is scaled by the factor `disp_unit` specified by the target
process, at window creation.
The following `info` keys are supported:
* `no_locks` : If set to *true*, then the implementation may assume that the local
:   If set to true, then the implementation may assume that the local
    window is never locked (by a call to MPI_Win_lock or
    MPI_Win_lock_all). Setting this value if only active synchronization
    may allow the implementation to enable certain optimizations.
```{=html}
* `accumulate_ordering` : By default, accumulate operations from one initiator to one target
:   By default, accumulate operations from one initiator to one target
    on the same window are strictly ordered. If the `info` key
    `accumulate_ordering` is set to none, no ordering of accumulate
    operations guaranteed. They key can also be a comma-separated list
    of required orderings consisting of rar, war, raw, and waw
    for read-after-read, write-after-read, read-after-write, and
    write-after-write, respectively. Looser ordering constraints are
    likely to result in improved performance.
```{=html}
* `accumulate_ops` : If set to *same_op*, the implementation will assume that all
:   If set to same_op, the implementation will assume that all
    concurrent accumulate calls to the same target address will use the
    same operation. If set to same_op_no_op, then the implementation
    will assume that all concurrent accumulate calls to the same target
    address will use the same operation or MPI_NO_OP. The default is
    same_op_no_op.
```{=html}
* `same_size` : If set to *true*, then the implementation may assume that the
:   If set to true, then the implementation may assume that the
    argument size is identical on all processes, and that all
    processes have provided this `info` key with the same value.
```{=html}
* `same_disp_unit` : If set to *true*, then the implementation may assume that the
:   If set to true, then the implementation may assume that the
    argument disp_unit is identical on all processes, and that all
    processes have provided this `info` key with the same value.

# Notes

Common choices for `disp_unit` are 1 (no scaling), and (in C syntax)
sizeof(type), for a window that consists of an array of elements of
type type. The later choice will allow one to use array indices in RMA
calls, and have those scaled correctly to byte displacements, even in a
heterogeneous environment.
Use memory allocated by `MPI_Alloc_mem` to guarantee properly aligned
window boundaries (such as word, double-word, cache line, page frame,
and so on).

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the SIZE
argument only for Fortran 90. FORTRAN 77 users may use the non-portable
syntax
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

[`MPI_Free_mem(3)`](./?file=MPI_Free_mem.md)
[`MPI_Win_allocate(3)`](./?file=MPI_Win_allocate.md)
[`MPI_Win_allocate_shared(3)`](./?file=MPI_Win_allocate_shared.md)
[`MPI_Alloc_mem(3)`](./?file=MPI_Alloc_mem.md)
