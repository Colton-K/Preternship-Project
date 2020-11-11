# Name

`MPI_File_read_all` - Reads a file starting at the locations specified
by individual file pointers (blocking, collective).

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_read_all(MPI_File fh, void *buf,

    int count, MPI_Datatype datatype, MPI_Status *status)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_READ_ALL(FH, BUF, COUNT,
    DATATYPE, STATUS, IERROR)
    <type>    BUF(*)

    INTEGER    FH, COUNT, DATATYPE, STATUS(MPI_STATUS_SIZE),IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_read_all(fh, buf, count, datatype, status, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    TYPE(*), DIMENSION(..) :: buf
    INTEGER, INTENT(IN) :: count
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    TYPE(MPI_Status) :: status
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `fh` : File handle (handle).
* `count` : Number of elements in buffer (integer).
* `datatype` : Data type of each buffer element (handle).

# Output Parameters

* `buf` : Initial address of buffer (choice).
* `status` : Status object (status).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_read_all` is a collective routine that attempts to read from the
file associated with `fh` (at the current individual file pointer
position maintained by the system) a total number of `count` data items
having `datatype` type into the user's buffer `buf`. The data is taken
out of those parts of the file specified by the current view.
`MPI_File_read_all` stores the number of data-type elements actually read
in `status`. All other fields of `status` are undefined.
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
