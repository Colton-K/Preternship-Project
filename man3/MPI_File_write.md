# Name

`MPI_File_write` - Writes a file starting at the location specified by
the individual file pointer (blocking, noncollective).

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_write(MPI_File fh, const void *buf,

    int count, MPI_Datatype datatype,

    MPI_Status *status)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_WRITE(FH, BUF, COUNT,
    DATATYPE, STATUS, IERROR)
    <type>    BUF(*)

    INTEGER    FH, COUNT, DATATYPE, STATUS(MPI_STATUS_SIZE), IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_write(fh, buf, count, datatype, status, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    TYPE(*), DIMENSION(..), INTENT(IN) :: buf
    INTEGER, INTENT(IN) :: count
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    TYPE(MPI_Status) :: status
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `fh` : File handle (handle).

# Input Parameters

* `buf` : Initial address of buffer (choice).
* `count` : Number of elements in buffer (integer).
* `datatype` : Data type of each buffer element (handle).

# Output Parameters

* `status` : Status object (status).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_write` attempts to write into the file associated with `fh` (at
the current individual file pointer position maintained by the system) a
total number of `count` data items having `datatype` type from the
user's buffer `buf`. The data is written into those parts of the file
specified by the current view. `MPI_File_write` stores the number of
`datatype` elements actually written in `status`. All other fields of
`status` are undefined.
It is erroneous to call this function if `MPI_MODE_SEQUENTIAL` mode was
specified when the file was opened.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
`MPI_ERRORS_RETURN`. The error handler may be changed with
`MPI_File_set_errhandler`; the predefined error handler
`MPI_ERRORS_ARE_FATAL` may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
