# Name

`MPI_File_seek_shared` - Updates the global shared file pointer
(collective).

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_seek_shared(MPI_File fh, MPI_Offset offset,
    int whence)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_SEEK_SHARED(FH, OFFSET, WHENCE, IERROR)
    INTEGER    FH, WHENCE, IERROR

    INTEGER(KIND=MPI_OFFSET_KIND)    OFFSET
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_seek_shared(fh, offset, whence, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
    INTEGER, INTENT(IN) :: whence
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `fh` : File handle (handle).
* `offset` : File offset (integer).
* `whence` : Update mode (integer).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_seek_shared` updates the shared file pointer according to
`whence`, which could have the following possible values:
* ` o` : MPI_SEEK_SET - The pointer is set to *offset.*
:   MPI_SEEK_SET - The pointer is set to offset.
* ` o` : MPI_SEEK_CUR - The pointer is set to the current pointer position
:   MPI_SEEK_CUR - The pointer is set to the current pointer position
    plus offset.
* ` o` : MPI_SEEK_END - The pointer is set to the end of the file plus
:   MPI_SEEK_END - The pointer is set to the end of the file plus
    offset.
`MPI_File_seek_shared` is collective; all the processes in the
communicator group associated with the file handle `fh` must call
`MPI_File_seek_shared` with the same `offset` and `whence`. All processes
in the communicator group are synchronized before the shared file
pointer is updated.
The `offset` can be negative, which allows seeking backwards. It is
erroneous to seek to a negative position in the view. The end of the
view is defined to be the position of the next elementary data item,
relative to the current view, following the last whole elementary data
item accessible.

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the OFFSET
argument only for Fortran 90. FORTRAN 77 users may use the non-portable
syntax
where `MPI_OFFSET_KIND` is a constant defined in mpif.h and gives the
length of the declared integer in bytes.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
`MPI_ERRORS_RETURN`. The error handler may be changed with
`MPI_File_set_errhandler`; the predefined error handler
`MPI_ERRORS_ARE_FATAL` may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
