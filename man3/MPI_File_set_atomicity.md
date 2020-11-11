# Name

`MPI_File_set_atomicity` - Sets consistency semantics for data-access
operations (collective).

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_set_atomicity(MPI_File fh, int flag)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_SET_ATOMICITY(FH, FLAG, IERROR)
    INTEGER    FH, FLAG, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_set_atomicity(fh, flag, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    LOGICAL, INTENT(IN) :: flag
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `fh` : File handle (handle).
* `flag` : **true** to enable atomic mode, **false** to enable nonatomic mode
:   true to enable atomic mode, false to enable nonatomic mode
(boolean).
```


# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

The consistency semantics for data-access operations using the set of
file handles created by one collective `MPI_File_open` is set by
collectively calling `MPI_File_set_atomicity`. All processes in the group
must pass identical values for `fh` and `flag`. If `flag` is true,
atomic mode is set; if `flag` is `false`, nonatomic mode is set.
The default value on a call to `MPI_File_open` in Open MPI is `true` for
jobs running on more than one node, `false` for jobs running on a single
SMP. For more information, see the MPI-2 standard.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
`MPI_ERRORS_RETURN`. The error handler may be changed with
`MPI_File_set_errhandler`; the predefined error handler
`MPI_ERRORS_ARE_FATAL` may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
