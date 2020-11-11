# Name

`MPI_Win_attach`, `MPI_Win_detach` - One-sided MPI call that attach /
detach a window object for RMA operations.

# Syntax

## C Syntax

```c
#include <mpi.h>

MPI_Win_attach(MPI_Win win, void *base, MPI_Aint size)

MPI_Win_detach(MPI_Win win, void *base)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_ATTACH(WIN, BASE, SIZE, IERROR)
    <type> BASE(*)

    INTEGER(KIND=MPI_ADDRESS_KIND) SIZE
    INTEGER WIN, IERROR

MPI_WIN_DETACH(WIN, BASE, IERROR)
    <type> BASE(*)
    INTEGER WIN, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_attach(win, base, size, ierror)
    TYPE(MPI_Win), INTENT(IN) :: win
    TYPE(*), DIMENSION(..), INTENT(IN) :: base
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: size
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Win_detach(win, base, ierror)
    TYPE(MPI_Win), INTENT(IN) :: win
    TYPE(*), DIMENSION(..), INTENT(IN) :: base
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `win` : A window that was created with *MPI_Win_create_dynamic*
* `base` : Initial address of window (choice).
* `size` : Size of window in bytes (nonnegative integer).

# Output Parameters

* `win` : Window object returned by the call (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_attach` is a one-sided MPI communication collective call executed
by all processes in the group of comm. It returns a window object that
can be used by these processes to perform RMA operations. Each process
specifies a window of existing memory that it exposes to RMA accesses by
the processes in the group of comm. The window consists of size
bytes, starting at address base. A process may elect to expose no
memory by specifying `size` = 0.
If the `base` value used by `MPI_Win_attach` was allocated by
`MPI_Alloc_mem`, the `size` of the window can be no larger than the value
set by the `MPI_ALLOC_MEM` function.

# Notes

Use memory allocated by `MPI_Alloc_mem` to guarantee properly aligned
window boundaries (such as word, double-word, cache line, page frame,
and so on).

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
