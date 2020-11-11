# Name

`MPI_Win_create_dynamic` - One-sided MPI call that returns a window
object for RMA operations.

# Syntax

## C Syntax

```c
#include <mpi.h>

MPI_Win_create_dynamic(MPI_Info info, MPI_Comm comm, MPI_Win *win)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_CREATE_DYNAMIC(INFO, COMM, WIN, IERROR)
    INTEGER INFO, COMM, WIN, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_create_dynamic(info, comm, win, ierror)
    TYPE(MPI_Info), INTENT(IN) :: info
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Win), INTENT(OUT) :: win
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `info` : Info argument (handle).
* `comm` : Communicator (handle).

# Output Parameters

* `win` : Window object returned by the call (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_create_dynamic` is a one-sided MPI communication collective call
executed by all processes in the group of comm. It returns a window
object without memory attached that can be used by these processes to
perform RMA operations.
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

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
