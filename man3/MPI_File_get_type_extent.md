# Name

`MPI_File_get_type_extent` - Returns the extent of the data type in a
file.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_get_type_extent(MPI_File fh, MPI_Datatype

    datatype, MPI_Aint *extent)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_GET_TYPE_EXTENT(FH, DATATYPE, EXTENT, IERROR)
    INTEGER    FH, DATATYPE, IERROR

    INTEGER(KIND=MPI_ADDRESS_KIND)    EXTENT
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_get_type_extent(fh, datatype, extent, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: extent
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `fh` : File handle (handle).
* `datatype` : Data type (handle).

# Output Parameters

* `extent` : Data type extent (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_get_type_extent` can be used to calculate `extent` for
`datatype` in the file. The `extent` is the same for all processes
accessing the file associated with fh. If the current view uses a
user-defined data representation, `MPI_File_get_type_extent` uses the
`dtype_file_extent_fn` callback to calculate the extent.

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the EXTENT
argument only for Fortran 90. FORTRAN 77 users may use the non-portable
syntax
where `MPI_ADDRESS_KIND` is a constant defined in mpif.h and gives the
length of the declared integer in bytes.

# Notes

If the file data representation is other than "native," care must be
taken in constructing etypes and file types. Any of the data-type
constructor functions may be used; however, for those functions that
accept displacements in bytes, the displacements must be specified in
terms of their values in the file for the file data representation being
used. MPI will interpret these byte displacements as is; no scaling will
be done. The function `MPI_File_get_type_extent` can be used to calculate
the extents of data types in the file. For etypes and file types that
are portable data types, MPI will scale any displacements in the data
types to match the file data representation. Data types passed as
arguments to read/write routines specify the data layout in memory;
therefore, they must always be constructed using displacements
corresponding to displacements in memory.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
`MPI_ERRORS_RETURN`. The error handler may be changed with
`MPI_File_set_errhandler`; the predefined error handler
`MPI_ERRORS_ARE_FATAL` may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
