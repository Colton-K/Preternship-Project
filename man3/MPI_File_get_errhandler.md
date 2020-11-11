# Name

MPI_File_get_errhandler  - Gets the error handler for a file.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_get_errhandler(MPI_File file, MPI_Errhandler
    *errhandler)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_GET_ERRHANDLER(FILE, ERRHANDLER, IERROR)
    INTEGER    FILE, ERRHANDLER, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_get_errhandler(file, errhandler, ierror)
    TYPE(MPI_File), INTENT(IN) :: file
    TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `file` : File (handle).

# Output Parameters

* `errhandler` : MPI error handler currently associated with file (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

Returns in `errhandler` (a handle to) the error handler that is
currently associated with `file` `file`.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
`MPI_ERRORS_RETURN`. The error handler may be changed with
`MPI_File_set_errhandler`; the predefined error handler
`MPI_ERRORS_ARE_FATAL` may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
