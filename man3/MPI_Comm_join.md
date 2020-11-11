# Name

`MPI_Comm_join` - Establishes communication between MPI jobs

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Comm_join(int fd, MPI_Comm *intercomm)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_JOIN(FD, INTERCOMM, IERROR)
    INTEGER    FD, INTERCOMM, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Comm_join(fd, intercomm, ierror)
    INTEGER, INTENT(IN) :: fd
    TYPE(MPI_Comm), INTENT(OUT) :: intercomm
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `fd` : socket file descriptor (socket).

# Output Parameters

* `intercomm` : Intercommunicator between processes (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Comm_join` creates an intercommunicator from the union of two MPI
processes that are connected by a socket. `fd` is a file descriptor
representing a socket of type `SOCK_STREAM` (a two-way reliable
byte-stream connection). Nonblocking I/O and asynchronous notification
via SIGIO must not be enabled for the socket. The socket must be in a
connected state, and must be quiescent when `MPI_Comm_join` is called.
`MPI_Comm_join` must be called by the process at each end of the socket.
It does not return until both processes have called `MPI_Comm_join.`

# Notes

There are no MPI library calls for opening and manipulating a socket.
The socket `fd` can be opened using standard socket API calls. MPI uses
the socket to bootstrap creation of the intercommunicator, and for
nothing else. Upon return, the file descriptor will be open and
quiescent.
In a multithreaded process, the application must ensure that other
threads do not access the socket while one is in the midst of calling
`MPI_Comm_join.`
The returned communicator will contain the two processes connected by
the socket, and may be used to establish MPI communication with
additional processes, through the usual MPI communicator-creation
mechanisms.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
See the MPI man page for a full list of MPI error codes.

# See Also

    socket(3SOCKET)
[`MPI_Comm_create(3)`](./?file=MPI_Comm_create.md)
[`MPI_Comm_group(3)`](./?file=MPI_Comm_group.md)
