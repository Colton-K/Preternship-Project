# Name

`MPI_Open_port` - Establishes a network address for a server to accept
connections from clients.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Open_port(MPI_Info info, char *port_name)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_OPEN_PORT(INFO, PORT_NAME, IERROR)
    CHARACTER*(*)    PORT_NAME
    INTEGER        INFO, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Open_port(info, port_name, ierror)
    TYPE(MPI_Info), INTENT(IN) :: info
    CHARACTER(LEN=MPI_MAX_PORT_NAME), INTENT(OUT) :: port_name
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `info` : Options on how to establish an address (handle). No options
currently supported.
```


# Output Parameters

* `port_name` : Newly established port (string).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Open_port` establishes a network address, encoded in the `port_name`
string, at which the server will be able to accept connections from
clients. `port_name` is supplied by the system.
MPI copies a system-supplied port name into `port_name`. `port_name`
identifies the newly opened port and can be used by a client to contact
the server. The maximum size string that may be supplied by the system
is `MPI_MAX_PORT_NAME.`

# Supported Info Keys

None.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

# See Also

[`MPI_Comm_connect(3)`](./?file=MPI_Comm_connect.md)
[`MPI_Comm_accept(3)`](./?file=MPI_Comm_accept.md)
