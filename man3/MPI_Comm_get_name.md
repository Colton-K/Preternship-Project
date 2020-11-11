# Name

`MPI_Comm_get_name` - Returns the name that was most recently
associated with a communicator.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Comm_get_name(MPI_Comm comm, char *comm_name, int *resultlen)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_GET_NAME(COMM, COMM_NAME, RESULTLEN, IERROR)
    INTEGER    COMM, RESULTLEN, IERROR 
    CHARACTER*(*) COMM_NAME
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Comm_get_name(comm, comm_name, resultlen, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    CHARACTER(LEN=MPI_MAX_OBJECT_NAME), INTENT(OUT) :: comm_name
    INTEGER, INTENT(OUT) :: resultlen
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `comm` : Communicator the name of which is to be returned (handle).

# Output Parameter

* `comm_name` : Name previously stored on the communicator, or an empty string if no
such name exists (string).
* `resultlen` : Length of returned name (integer).
* `IERROR` : Fortran only: Error status (integer).
```


# Description

`MPI_Comm_get_name` returns the last name that was previously associated
with the given communicator. The name may be set and retrieved from any
language. The same name will be returned independent of the language
used. `comm_name` should be allocated so that it can hold a resulting
string of length `MPI_MAX_OBJECT_NAME` characters. `MPI_Comm_get_name`
returns a copy of the set name in `comm_name.`
If the user has not associated a name with a communicator, or an error
occurs, `MPI_Comm_get_name` will return an empty string (all spaces in
Fortran, "" in C). The three predefined communicators will have
predefined names associated with them. Thus, the names of
`MPI_COMM_WORLD`, `MPI_COMM_SELF`, and `MPI_COMM_PARENT` will have the default
of `MPI_COMM_WORLD`, `MPI_COMM_SELF`, and `MPI_COMM_PARENT`. The fact that the
system may have chosen to give a default name to a communicator does not
prevent the user from setting a name on the same communicator; doing
this removes the old name and assigns the new one.

# Notes

It is safe simply to print the string returned by `MPI_Comm_get_name`, as
it is always a valid string even if there was no name.
Note that associating a name with a communicator has no effect on the
semantics of an MPI program, and will (necessarily) increase the store
requirement of the program, since the names must be saved. Therefore,
there is no requirement that users use these functions to associate
names with communicators. However debugging and profiling MPI
applications may be made easier if names are associated with
communicators, since the debugger or profiler should then be able to
present information in a less cryptic manner.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
