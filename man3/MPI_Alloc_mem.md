# Name

MPI_Alloc_mem  - Allocates a specified memory segment.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr)
```

## Fortran Syntax (See Fortran Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_ALLOC_MEM(SIZE, INFO, BASEPTR, IERROR)
    INTEGER INFO, IERROR

    INTEGER(KIND=MPI_ADDRESS_KIND) SIZE, BASEPTR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Alloc_mem(size, info, baseptr, ierror)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY 
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: size
    TYPE(MPI_Info), INTENT(IN) :: info
    TYPE(C_PTR), INTENT(OUT) :: baseptr
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `size` : Size of memory segment in bytes (nonnegative integer).
* `info` : Info argument (handle).

# Output Parameters

* `baseptr` : Pointer to beginning of memory segment allocated.
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Alloc_mem` allocates `size` bytes of memory. The starting address of
this memory is returned in the variable base.

# Fortran Notes

There is no portable FORTRAN 77 syntax for using `MPI_Alloc_mem`. There is
no portable Fortran syntax for using pointers returned from
`MPI_Alloc_mem`. However, `MPI_Alloc_mem` can be used with Sun Fortran
compilers.
From FORTRAN 77, you can use the following non-standard declarations for
the SIZE and BASEPTR arguments:
               INCLUDE "mpif.h"
From either FORTRAN 77 or Fortran 90, you can use "Cray pointers" for
the BASEPTR argument. Cray pointers are described further in the Fortran
User's Guide and are supported by many Fortran compilers. For example,
               INCLUDE "mpif.h"
               ! use A

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
