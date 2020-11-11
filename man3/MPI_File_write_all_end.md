# Name

`MPI_File_write_all_end` - Writes a file starting at the locations
specified by individual file pointers; ending part of a split collective
routine (blocking).

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_write_all_end(MPI_File fh, const void *buf, MPI_Status *status)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_WRITE_ALL_END(FH, BUF, STATUS, IERROR)
    <type>    BUF(*)
    INTEGER    FH, STATUS, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_write_all_end(fh, buf, status, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
    TYPE(MPI_Status) :: status
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `fh` : File handle (handle).

# Input Parameter

* `buf` : Initial address of buffer (choice).

# Output Parameters

* `status` : Status object (status).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_write_all_end` is the ending part of a split collective routine
that stores the number of elements actually written into the file
associated with `fh` from the user's buffer `buf` in status.
`MPI_File_write_all_end` blocks until the operation initiated by
`MPI_File_write_all_begin` completes. The data is written into those parts
of the file specified by the current view. All other fields of status
are undefined.

# Notes

All the nonblocking collective routines for data access are "split"
into two routines, each with `_begin` or `_end` as a suffix. These split
collective routines are subject to the semantic rules described in
Section 9.4.5 of the MPI-2 standard.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
`MPI_ERRORS_RETURN`. The error handler may be changed with
`MPI_File_set_errhandler`; the predefined error handler
`MPI_ERRORS_ARE_FATAL` may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
