# Name

`MPI_Win_allocate` - One-sided MPI call that allocates memory and
returns a window object for RMA operations.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_allocate (MPI_Aint size, int disp_unit, MPI_Info info,

                      MPI_Comm comm, void *baseptr, MPI_Win *win)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_ALLOCATE(SIZE, DISP_UNIT, INFO, COMM, BASEPTR, WIN, IERROR)

    INTEGER(KIND=MPI_ADDRESS_KIND) SIZE, BASEPTR
    INTEGER DISP_UNIT, INFO, COMM, WIN, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_allocate(size, disp_unit, info, comm, baseptr, win, ierror)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: size
    INTEGER, INTENT(IN) :: disp_unit
    TYPE(MPI_Info), INTENT(IN) :: info
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(C_PTR), INTENT(OUT) :: baseptr
    TYPE(MPI_Win), INTENT(OUT) :: win
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `size` : Size of window in bytes (nonnegative integer).
* `disp_unit` : Local unit size for displacements, in bytes (positive integer).
* `info` : Info argument (handle).
* `comm` : Communicator (handle).

# Output Parameters

* `baseptr` : Initial address of window.
* `win` : Window object returned by the call (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_allocate` is a collective call executed by all processes in
the group of comm. On each process, it allocates memory of at least
`size` bytes, returns a pointer to it, and returns a window object that
can be used by all processes in `comm` to perform RMA operations. The
returned memory consists of `size` bytes local to each process, starting
at address `baseptr` and is associated with the window as if the user
called `MPI_Win_create` on existing memory. The `size` argument may be
different at each process and `size` = 0 is valid; however, a library
might allocate and expose more memory in order to create a fast,
globally symmetric allocation. The discussion of and rationales for
`MPI_Alloc_mem` and `MPI_Free_mem` in MPI-3.1 � 8.2 also apply to
`MPI_Win_allocate`; in particular, see the rationale in MPI-3.1 � 8.2
for an explanation of the type used for baseptr.
The displacement unit argument is provided to facilitate address
arithmetic in RMA operations: the target displacement argument of an RMA
operation is scaled by the factor `disp_unit` specified by the target
process, at window creation.
For supported `info` keys see `MPI_Win_create.`

# Notes

Common choices for `disp_unit` are 1 (no scaling), and (in C syntax)
sizeof(type), for a window that consists of an array of elements of
type type. The later choice will allow one to use array indices in RMA
calls, and have those scaled correctly to byte displacements, even in a
heterogeneous environment.

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
[`MPI_Win_create(3)`](./?file=MPI_Win_create.md)
[`MPI_Win_allocate_shared(3)`](./?file=MPI_Win_allocate_shared.md)
[`MPI_Alloc_mem(3)`](./?file=MPI_Alloc_mem.md)
