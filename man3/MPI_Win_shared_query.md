# Name

`MPI_Win_shared_query` - Query a shared memory window

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_shared_query (MPI_Win win, int rank, MPI_Aint *size,
                          int *disp_unit, void *baseptr)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_SHARED_QUERY(WIN, RANK, SIZE, DISP_UNIT, BASEPTR, IERROR)
        INTEGER WIN, RANK, DISP_UNIT, IERROR

        INTEGER(KIND=MPI_ADDRESS_KIND) SIZE, BASEPTR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_shared_query(win, rank, size, disp_unit, baseptr, ierror)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    TYPE(MPI_Win), INTENT(IN) :: win
    INTEGER, INTENT(IN) :: rank
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: size
    INTEGER, INTENT(OUT) :: disp_unit
    TYPE(C_PTR), INTENT(OUT) :: baseptr
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `win` : Shared memory window object (handle).
* `rank` : Rank in the group of window *win* (non-negative integer) or

MPI_PROC_NULL.
```


# Output Parameters

* `size` : Size of the window segment (non-negative integer).
* `disp_unit` : Local unit size for displacements, in bytes (positive integer).
* `baseptr` : Address for load/store access to window segment (choice).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Win_shared_query` queries the process-local address for remote
memory segments created with `MPI_Win_allocate_shared`. This function can
return different process-local addresses for the same physical memory on
different processes. The returned memory can be used for load/store
accesses subject to the constraints defined in MPI-3.1 � 11.7. This
function can only be called with windows of flavor
`MPI_WIN_FLAVOR_SHARED`. If the passed window is not of flavor
`MPI_WIN_FLAVOR_SHARED`, the error `MPI_ERR_RMA_FLAVOR` is raised. When rank
is `MPI_PROC_NULL`, the pointer, `disp_unit`, and `size` returned are
the pointer, `disp_unit`, and `size` of the memory segment belonging the
lowest `rank` that specified `size` > 0. If all processes in the group
attached to the window specified `size` = 0, then the call returns
size = 0 and a `baseptr` as if `MPI_Alloc_mem` was called with
`size` = 0.

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

[`MPI_Win_allocate_shared(3)`](./?file=MPI_Win_allocate_shared.md)
[`MPI_Alloc_mem(3)`](./?file=MPI_Alloc_mem.md)
