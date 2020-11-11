# Name

`MPI_File_set_size` - Resizes a file (collective).

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_set_size(MPI_File fh, MPI_Offset size)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_SET_SIZE(FH, SIZE, IERROR)
    INTEGER    FH, IERROR

    INTEGER(KIND=MPI_OFFSET_KIND)    SIZE
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_set_size(fh, size, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: size
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `fh` : File handle (handle).
* `size` : Size to truncate or expand file (integer).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_set_size` resizes the file associated with the file handle fh,
truncating UNIX files as necessary. `MPI_File_set_size` is collective; all
processes in the group must pass identical values for size.
When using `MPI_File_set_size` on a UNIX file, if `size` is larger than
the current file `size`, the file `size` becomes `size`. If ``size`` is
smaller than the current file size, the file is truncated at the
position defined by `size` (from the beginning of the file and measured
in bytes). Regions of the file which have been previously written are
unaffected.
`MPI_File_set_size` does not affect the individual file pointers or the
shared file pointer.
Note that the actual amount of storage space cannot be allocated by
`MPI_File_set_size`. Use `MPI_File_preallocate` to accomplish this.
It is erroneous to call this function if `MPI_MODE_SEQUENTIAL` mode was
specified when the file was opened.

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the SIZE
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
