# Name

`MPI_File_preallocate` - Preallocates a specified amount of storage
space at the beginning of a file (collective).

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_preallocate(MPI_File fh, MPI_Offset size)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_PREALLOCATE(FH, SIZE, IERROR)
    INTEGER    FH, IERROR

    INTEGER(KIND=MPI_OFFSET_KIND)    SIZE
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_preallocate(fh, size, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: size
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `fh` : File handle (handle).

# Input Parameter

* `size` : Size to preallocate file, in bytes (integer).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_preallocate` ensures that storage space is allocated for the
first `size` bytes of the file associated with fh.
`MPI_File_preallocate` can be a very time-consuming operation.
`MPI_File_preallocate` is collective; all processes in the group must pass
identical values for size. Regions of the file that have previously
been written are unaffected. For newly allocated regions of the file,
`MPI_File_preallocate` has the same effect as writing undefined data. If
`size` is larger than the current file `size`, the file `size` increases to
size. If `size` is less than or equal to the current file size, the
file `size` is unchanged.
The treatment of file pointers, pending nonblocking accesses, and file
consistency is the same as with `MPI_File_set_size`. If
`MPI_MODE_SEQUENTIAL` mode was specified when the file was opened, it is
erroneous to call this routine.

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the SIZE
argument only for Fortran 90. FORTRAN 77 users may use the non-portable
syntax
where `MPI_OFFSET_KIND` is a constant defined in mpif.h and gives the
length of the declared integer in bytes.

# Notes

When using the collective routine `MPI_File_set_size` on a UNIX file, if
the `size` that is set is smaller than the current file `size`, the file is
truncated at the position defined by `size`. If the `size` is set to be
larger than the current file `size`, the file `size` becomes the set `size`.
When the file `size` is increased this way with `MPI_File_set_`size``, new
regions are created in the file with displacements between the old file
size and the larger, newly set file size.
Sun MPI I/O does not necessarily allocate file space for such new
regions. You may reserve file space either by using `MPI_File_preallocate`
or by performing a read or write to certain bytes.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
`MPI_ERRORS_RETURN`. The error handler may be changed with
`MPI_File_set_errhandler`; the predefined error handler
`MPI_ERRORS_ARE_FATAL` may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
