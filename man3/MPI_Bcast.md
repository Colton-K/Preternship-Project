# Name

`MPI_Bcast`, `MPI_Ibcast` - Broadcasts a message from the process with
rank `root` to all other processes of the group.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Bcast(void *buffer, int count, MPI_Datatype datatype,

  int root, MPI_Comm comm)

int MPI_Ibcast(void *buffer, int count, MPI_Datatype datatype,

  int root, MPI_Comm comm, MPI_Request *request)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_BCAST(BUFFER, COUNT, DATATYPE, ROOT, COMM, IERROR)
  <type>  BUFFER(*)
  INTEGER COUNT, DATATYPE, ROOT, COMM, IERROR

MPI_IBCAST(BUFFER, COUNT, DATATYPE, ROOT, COMM, REQUEST, IERROR)
  <type>  BUFFER(*)
  INTEGER COUNT, DATATYPE, ROOT, COMM, REQUEST, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Bcast(buffer, count, datatype, root, comm, ierror)
  TYPE(*), DIMENSION(..) :: buffer
  INTEGER, INTENT(IN) :: count, root
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Ibcast(buffer, count, datatype, root, comm, request, ierror)
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buffer
  INTEGER, INTENT(IN) :: count, root
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameters

* `buffer` : Starting address of buffer (choice).
* `count` : Number of entries in buffer (integer).
* `datatype` : Data type of buffer (handle).
* `root` : Rank of broadcast root (integer).
* `comm` : Communicator (handle).

# Output Parameters

* `request` : Request (handle, non-blocking only).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Bcast` broadcasts a message from the process with rank `root` to all
processes of the group, itself included. It is called by all members of
group using the same arguments for comm, root. On return, the contents
of root's communication `buffer` has been copied to all processes.
General, derived datatypes are allowed for datatype. The type signature
of count, `datatype` on any process must be equal to the type signature of
count, `datatype` at the root. This implies that the amount of data sent
must be equal to the amount received, pairwise between each process and
the root. `MPI_Bcast` and all other data-movement collective routines make
this restriction. Distinct type maps between sender and receiver are
still allowed.
Example: Broadcast 100 ints from process 0 to every process in the
group.
        MPI_Comm comm;
        int array[100];
        int root=0;
        MPI_Bcast( array, 100, MPI_INT, root, comm);
As in many of our sample code fragments, we assume that some of the
variables (such as `comm` in the example above) have been assigned
appropriate values.

# When Communicator Is An Inter-Communicator

When the communicator is an inter-communicator, the `root` process in the
first group broadcasts data to all the processes in the second group.
The first group defines the `root` process. That process uses `MPI_ROOT` as
the value of its `root` argument. The remaining processes use
`MPI_PROC_NULL` as the value of their `root` argument. All processes in
the second group use the rank of that `root` process in the first group as
the value of their `root` argument. The receive `buffer` arguments of the
processes in the second group must be consistent with the send buffer
argument of the `root` process in the first group.

# Notes

This function does not support the in-place option.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
