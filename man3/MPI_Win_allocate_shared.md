# Name

`MPI_Win_allocate_shared` - One-sided MPI call that allocates shared
memory and returns a window object for RMA operations.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_allocate_shared (MPI_Aint size, int disp_unit, MPI_Info info,

                             MPI_Comm comm, void *baseptr, MPI_Win *win)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_ALLOCATE_SHARED(SIZE, DISP_UNIT, INFO, COMM, BASEPTR, WIN, IERROR)

    INTEGER(KIND=MPI_ADDRESS_KIND) SIZE, BASEPTR
    INTEGER DISP_UNIT, INFO, COMM, WIN, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_allocate_shared(size, disp_unit, info, comm, baseptr, win, ierror)
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

`MPI_Win_allocate_shared` is a collective call executed by all
processes in the group of comm. On each process, it allocates memory
of at least `size` bytes that is shared among all processes in comm,
and returns a pointer to the locally allocated segment in `baseptr` that
can be used for load/store accesses on the calling process. The locally
allocated memory can be the target of load/store accesses by remote
processes; the base pointers for other processes can be queried using
the function `MPI_Win_shared_query`. The call also returns a window
object that can be used by all processes in `comm` to perform RMA
operations. The `size` argument may be different at each process and
`size` = 0 is valid. It is the user's responsibility to ensure that the
communicator `comm` represents a group of processes that can create a
shared memory segment that can be accessed by all processes in the
group. The discussions of rationales for `MPI_Alloc_mem` and
`MPI_Free_mem` in MPI-3.1 � 8.2 also apply to
`MPI_Win_allocate_shared`; in particular, see the rationale in MPI-3.1
� 8.2 for an explanation of the type used for baseptr. The allocated
memory is contiguous across process ranks unless the `info` key
`alloc_shared_noncontig` is specified. Contiguous across process ranks
means that the first address in the memory segment of process i is
consecutive with the last address in the memory segment of process i -
1. This may enable the user to calculate remote address offsets with
local information only.
The following `info` keys are supported:
* `alloc_shared_noncontig` : If not set to *true*, the allocation strategy is to allocate
:   If not set to true, the allocation strategy is to allocate
    contiguous memory across process ranks. This may limit the
    performance on some architectures because it does not allow the
    implementation to modify the data layout (e.g., padding to reduce
    access latency).
```{=html}
* `blocking_fence` : If set to *true*, the osc/sm component will use **MPI_Barrier** for
:   If set to true, the osc/sm component will use MPI_Barrier for
    MPI_Win_fence. If set to false a condition variable and
    counter will be used instead. The default value is false. This
    `info` key is Open MPI specific.
```{=html}
For additional supported `info` keys see `MPI_Win_create.`

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
[`MPI_Win_allocate(3)`](./?file=MPI_Win_allocate.md)
[`MPI_Win_create(3)`](./?file=MPI_Win_create.md)
[`MPI_Alloc_mem(3)`](./?file=MPI_Alloc_mem.md)
[`MPI_Win_shared_query(3)`](./?file=MPI_Win_shared_query.md)
