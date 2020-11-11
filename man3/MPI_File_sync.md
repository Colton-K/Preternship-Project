# Name

`MPI_File_sync` - Makes semantics consistent for data-access
operations (collective).

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_sync(MPI_File fh)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_SYNC(FH, IERROR)
    INTEGER    FH, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_sync(fh, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `fh` : File handle (handle).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

Calling `MPI_File_sync` with `fh` causes all previous writes to `fh` by
the calling process to be written to permanent storage. If other
processes have made updates to permanent storage, then all such updates
become visible to subsequent reads of `fh` by the calling process.
`MPI_File_sync` is a collective operation. The user is responsible for
ensuring that all nonblocking requests on `fh` have been completed
before calling `MPI_File_sync`. Otherwise, the call to `MPI_File_sync` is
erroneous.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
`MPI_ERRORS_RETURN`. The error handler may be changed with
`MPI_File_set_errhandler`; the predefined error handler
`MPI_ERRORS_ARE_FATAL` may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
