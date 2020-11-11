# Name

`MPI_File_iread_shared` - Reads a file using the shared file pointer
(nonblocking, noncollective).

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_iread_shared(MPI_File fh, void *buf, int count,

    MPI_Datatype datatype, MPI_Request *request)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_IREAD_SHARED(FH, BUF, COUNT, DATATYPE, REQUEST, IERROR)
    <type>    BUF(*)
    INTEGER    FH, COUNT, DATATYPE, REQUEST, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_iread_shared(fh, buf, count, datatype, request, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
    INTEGER, INTENT(IN) :: count
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    TYPE(MPI_Request), INTENT(OUT) :: request
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `fh` : File handle (handle).

# Input Parameters

* `count` : Number of elements in buffer (integer).
* `datatype` : Data type of each buffer element (handle).

# Output Parameters

* `buf` : Initial address of buffer (choice).
* `request` : Request object (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_iread_shared` is a nonblocking version of the
`MPI_File_read_shared` interface. It uses the shared file pointer to read
files. The order of serialization among the processors is not
deterministic for this noncollective routine, so you need to use other
methods of synchronization to impose a particular order among
processors.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
`MPI_ERRORS_RETURN`. The error handler may be changed with
`MPI_File_set_errhandler`; the predefined error handler
`MPI_ERRORS_ARE_FATAL` may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
