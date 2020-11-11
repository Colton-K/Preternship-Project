# Name

`MPI_File_get_info` - Returns a new info object containing values for
current hints associated with a file.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_get_info(MPI_File fh, MPI_Info *info_used)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_GET_INFO(FH, INFO_USED, IERROR)
    INTEGER    FH, INFO_USED, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_get_info(fh, info_used, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    TYPE(MPI_Info), INTENT(OUT) :: info_used
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `fh` : File handle (handle).

# Output Parameters

* `info_used` : New info object (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_get_info` returns a new info object containing all the hints
that the system currently associates with the file fh. The current
setting of all hints actually used by the system related to this open
file is returned in `info_used`. The user is responsible for freeing
`info_used` via `MPI_Info_free.`
Note that the set of hints returned in `info_used` may be greater or
smaller than the set of hints passed in to `MPI_File_open,`
`MPI_File_set_view`, and `MPI_File_set_info`, as the system may not
recognize some hints set by the user, and may automatically set other
hints that the user has not requested to be set. See the HINTS section
for a list of hints that can be set.

# Hints

The following hints can be used as values for the `info_used` argument.
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

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
`MPI_ERRORS_RETURN`. The error handler may be changed with
`MPI_File_set_errhandler`; the predefined error handler
`MPI_ERRORS_ARE_FATAL` may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
