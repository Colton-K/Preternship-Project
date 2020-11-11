# Name

`MPI_File_write_all_begin` - Writes a file starting at the locations
specified by individual file pointers; beginning part of a split
collective routine (nonblocking).

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_write_all_begin(MPI_File fh, const void *buf,

    int count, MPI_Datatype datatype)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_WRITE_ALL_BEGIN(FH, BUF, COUNT, DATATYPE, IERROR)
    <type>    BUF(*)
    INTEGER    FH, COUNT, DATATYPE, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_write_all_begin(fh, buf, count, datatype, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
    INTEGER, INTENT(IN) :: count
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `fh` : File handle (handle).

# Input Parameters

* `buf` : Initial address of buffer (choice).
* `count` : Number of elements in buffer (integer).
* `datatype` : Data type of each buffer element (handle).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_write_all_begin` is the beginning part of a split collective,
nonblocking routine that attempts to write into the file associated with
`fh` (at the current individual file pointer position maintained by the
system) a total number of `count` data items having `datatype` type from
the user's buffer `buf`. The data is written into those parts of the
file specified by the current view.

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