# Name

`MPI_File_open` - Opens a file (collective).

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_open(MPI_Comm comm, const char *filename,

    int amode, MPI_Info info,

    MPI_File *fh)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_OPEN(COMM, FILENAME, AMODE, INFO, FH, IERROR)
    CHARACTER*(*)    FILENAME
    INTEGER    COMM, AMODE, INFO, FH, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_open(comm, filename, amode, info, fh, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER, INTENT(IN) :: amode
    TYPE(MPI_Info), INTENT(IN) :: info
    TYPE(MPI_File), INTENT(OUT) :: fh
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `comm` : Communicator (handle).
* `filename` : Name of file to open (string).
* `amode` : File access mode (integer).
* `info` : Info object (handle).

# Output Parameters

* `fh` : New file handle (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_open` opens the file identified by the `filename` ``filename`` on
all processes in the `comm` communicator group. `MPI_File_open` is a
collective routine; all processes must provide the same value for
`amode`, and all processes must provide filenames that reference the
same file which are textually identical (note: Open MPI I/O plugins may
have restrictions on characters that can be used in filenames. For
example, the ROMIO plugin may disallow the colon (":") character from
appearing in a filename). A process can open a file independently of
other processes by using the `MPI_COMM_SELF` communicator. The file handle
returned, `fh`, can be subsequently used to access the file until the
file is closed using `MPI_File_close`. Before calling `MPI_Finalize`, the
user is required to close (via `MPI_File_close`) all files that were
opened with `MPI_File_open`. Note that the communicator `comm` is
unaffected by `MPI_File_open` and continues to be usable in all MPI
routines. Furthermore, use of `comm` will not interfere with I/O
behavior.
Initially, all processes view the file as a linear byte stream; that is,
the `etype` and `filetype` are both `MPI_BYTE`. The file view can be
changed via the `MPI_File_set_view` routine.
The following access modes are supported (specified in amode, in a
bit-vector OR in one of the following integer constants):
* ` o` : MPI_MODE_APPEND
* ` o` : MPI_MODE_CREATE -- Create the file if it does not exist.
:   MPI_MODE_CREATE -- Create the file if it does not exist.
* ` o` : MPI_MODE_DELETE_ON_CLOSE
* ` o` : MPI_MODE_EXCL -- Error creating a file that already exists.
:   MPI_MODE_EXCL -- Error creating a file that already exists.
* ` o` : MPI_MODE_RDONLY -- Read only.
:   MPI_MODE_RDONLY -- Read only.
* ` o` : MPI_MODE_RDWR -- Reading and writing.
:   MPI_MODE_RDWR -- Reading and writing.
* ` o` : MPI_MODE_SEQUENTIAL
* ` o` : MPI_MODE_WRONLY -- Write only.
:   MPI_MODE_WRONLY -- Write only.
* ` o` : MPI_MODE_UNIQUE_OPEN
The modes `MPI_MODE_RDONLY`, `MPI_MODE_RDWR`, `MPI_MODE_WRONLY`, and
`MPI_MODE_CREATE` have identical semantics to their POSIX counterparts. It
is erroneous to specify `MPI_MODE_CREATE` in conjunction with
`MPI_MODE_RDONLY`. Errors related to the access mode are raised in the
class `MPI_ERR_AMODE.`
On single-node clusters, files are opened by default using nonatomic
mode file consistency semantics. The more stringent atomic-mode
consistency semantics, required for atomicity of overlapping accesses,
are the default when processors in a communicator group reside on more
than one node. This setting can be changed using `MPI_File_set_atomicity.`
The `MPI_File_open` interface allows the user to pass information via the
`info` argument. It can be set to `MPI_INFO_NULL`. See the HINTS section
for a list of hints that can be set.

# Hints

The following hints can be used as values for the `info` argument.
- `shared_file_timeout`: Amount of time (in seconds) to wait for access
to the shared file pointer before exiting with `MPI_ERR_TIMEDOUT.`
- `rwlock_timeout`: Amount of time (in seconds) to wait for obtaining a
read or write lock on a contiguous chunk of a UNIX file before exiting
with `MPI_ERR_TIMEDOUT.`
- `noncoll_read_bufsize`: Maximum size of the buffer used by MPI I/O to
satisfy multiple noncontiguous read requests in the noncollective
data-access routines. (See NOTE, below.)
- `noncoll_write_bufsize`: Maximum size of the buffer used by MPI I/O to
satisfy multiple noncontiguous write requests in the noncollective
data-access routines. (See NOTE, below.)
- `coll_read_bufsize`: Maximum size of the buffer used by MPI I/O to
satisfy multiple noncontiguous read requests in the collective
data-access routines. (See NOTE, below.)
- `coll_write_bufsize`: Maximum size of the buffer used by MPI I/O to
satisfy multiple noncontiguous write requests in the collective
data-access routines. (See NOTE, below.)
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
