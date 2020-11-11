# Name

`MPI_File_set_view` - Changes process's view of data in file
(collective).

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_set_view(MPI_File fh, MPI_Offset disp,

    MPI_Datatype etype, MPI_Datatype filetype,

    const char *datarep, MPI_Info info)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_SET_VIEW(FH, DISP, ETYPE,
    FILETYPE, DATAREP, INFO, IERROR)
    INTEGER    FH, ETYPE, FILETYPE, INFO, IERROR
    CHARACTER*(*)    DATAREP

    INTEGER(KIND=MPI_OFFSET_KIND)    DISP
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_set_view(fh, disp, etype, filetype, datarep, info, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: disp
    TYPE(MPI_Datatype), INTENT(IN) :: etype, filetype
    CHARACTER(LEN=*), INTENT(IN) :: datarep
    TYPE(MPI_Info), INTENT(IN) :: info
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `fh` : File handle (handle).

# Input Parameters

* `disp` : Displacement (integer).
* `etype` : Elementary data type (handle).
* `filetype` : File type (handle). See Restrictions, below.
* `datarep` : Data representation (string).
* `info` : Info object (handle).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

The `MPI_File_set_view` routine changes the process's view of the data in
the file -- the beginning of the data accessible in the file through
that view is set to `disp`; the type of data is set to `etype`; and the
distribution of data to processes is set to `filetype`. In addition,
`MPI_File_set_view` resets the independent file pointers and the shared
file pointer to zero. `MPI_File_set_view` is collective across the fh;
all processes in the group must pass identical values for `datarep` and
provide an `etype` with an identical extent. The values for disp,
filetype, and `info` may vary. It is erroneous to use the shared file
pointer data-access routines unless identical values for `disp` and
`filetype` are also given. The data types passed in `etype` and
`filetype` must be committed.
The `disp` displacement argument specifies the position (absolute offset
in bytes from the beginning of the file) where the view begins.
The `MPI_File_set_view` interface allows the user to pass a
data-representation string to MPI I/O via the `datarep` argument. To
obtain the default value (or "native"), pass NULL. The user can also
pass information via the `info` argument. See the HINTS section for a
list of hints that can be set. For more information, see the MPI-2
standard.

# Hints

The following hints can be used as values for the `info` argument.
- `shared_file_timeout`: Amount of time (in seconds) to wait for access
to the shared file pointer before exiting with `MPI_ERR_TIMEDOUT.`
- `rwlock_timeout`: Amount of time (in seconds) to wait for obtaining a
read or write lock on a contiguous chunk of a UNIX file before exiting
with `MPI_ERR_TIMEDOUT.`
- `noncoll_read_bufsize`: Maximum size of the buffer used by MPI I/O to
satisfy read requests in the noncollective data-access routines. (See
NOTE, below.)
- `noncoll_write_bufsize`: Maximum size of the buffer used by MPI I/O to
satisfy write requests in the noncollective data-access routines. (See
NOTE, below.)
- `coll_read_bufsize`: Maximum size of the buffer used by MPI I/O to
satisfy read requests in the collective data-access routines. (See NOTE,
below.)
- `coll_write_bufsize`: Maximum size of the buffer used by MPI I/O to
satisfy write requests in the collective data-access routines. (See
NOTE, below.)
NOTE: A buffer size smaller than the distance (in bytes) in a UNIX file
between the first byte and the last byte of the access request causes
MPI I/O to iterate and perform multiple UNIX read() or write() calls. If
the request includes multiple noncontiguous chunks of data, and the
buffer size is greater than the size of those chunks, then the UNIX
read() or write() (made at the MPI I/O level) will access data not
requested by this process in order to reduce the total number of write()
calls made. If this is not desirable behavior, you should reduce this
buffer size to equal the size of the contiguous chunks within the
aggregate request.
- `mpiio_concurrency`: (boolean) controls whether nonblocking I/O
routines can bind an extra thread to an LWP.
- `mpiio_coll_contiguous`: (boolean) controls whether subsequent
collective data accesses will request collectively contiguous regions of
the file.
- filename: Access this hint to get the name of the file.

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the DISP
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
