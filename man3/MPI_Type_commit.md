# Name

`MPI_Type_commit` - Commits a data type.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Type_commit(MPI_Datatype *datatype)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_TYPE_COMMIT(DATATYPE, IERROR)
    INTEGER    DATATYPE, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Type_commit(datatype, ierror)
    TYPE(MPI_Datatype), INTENT(INOUT) :: datatype
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `datatype` : Data type (handle).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

The commit operation commits the data type. A data type is the formal
description of a communication buffer, not the content of that buffer.
After a data type has been committed, it can be repeatedly reused to
communicate the changing content of a buffer or, indeed, the content of
different buffers, with different starting addresses.
Example: The following Fortran code fragment gives examples of using
`MPI_Type_commit.`
        INTEGER type1, type2
        CALL MPI_TYPE_CONTIGUOUS(5, MPI_REAL, type1, ierr)
                      ! new type object created
        CALL MPI_TYPE_COMMIT(type1, ierr)
                      ! now type1 can be used for communication
If the data type specified in `datatype` is already committed, it is
equivalent to a no-op.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
