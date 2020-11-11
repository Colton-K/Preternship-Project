# Name

`MPI_Status_set_elements`, `MPI_Status_set_elements_x` - Modifies
opaque part of `status` to allow `MPI_Get_elements` to return count.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Status_set_elements(MPI_Status *status, MPI_Datatype datatype, int count)

int MPI_Status_set_elements_x(MPI_Status *status, MPI_Datatype datatype, MPI_Count count)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_STATUS_SET_ELEMENTS(STATUS, DATATYPE, COUNT, IERROR)

    INTEGER    STATUS(MPI_STATUS_SIZE), DATATYPE, COUNT, IERROR

MPI_STATUS_SET_ELEMENTS_X(STATUS, DATATYPE, COUNT, IERROR)

    INTEGER    STATUS(MPI_STATUS_SIZE), DATATYPE

        INTEGER(KIND=MPI_COUNT_KIND) COUNT
        INTEGER IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Status_set_elements(status, datatype, count, ierror)
    TYPE(MPI_Status), INTENT(INOUT) :: status
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    INTEGER, INTENT(IN) :: count
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Status_set_elements_x(status, datatype, count, ierror)
    TYPE(MPI_Status), INTENT(INOUT) :: status
    TYPE(MPI_Datatype), INTENT(IN) :: datatype

    INTEGER(KIND = MPI_COUNT_KIND), INTENT(IN) :: count
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `status` : Status to associate with *count (status).*

# Input Parameters

* `datatype` : Data type associated with *count (handle).*
* `count` : Number of elements to associate with *status (integer).*

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Status_set_elements` modifies the opaque part of status so that a
call to `MPI_Get_elements` or `MPI_Get_elements_x` will return count.
`MPI_Get_count` will return a compatible value.
A subsequent call to `MPI_Get_count(status`, datatype, count), to
`MPI_Get_elements(status`, datatype, count), or to
`MPI_Get_elements_x(status`, datatype, count) must use a data-type
argument that has the same type signature as the data-type argument that
was used in the call to `MPI_Status_set_elements.`

# Notes

Users are advised not to reuse the `status` fields for values other than
those for which they were intended. Doing so may lead to unexpected
results when using the `status` object. For example, calling
`MPI_Get_elements` may cause an error if the value is out of range, or it
may be impossible to detect such an error. The `extra_state` argument
provided with a generalized request can be used to return information
that does not logically belong in status. Furthermore, modifying the
values in a `status` set internally by MPI, such as `MPI_Recv`, may lead to
unpredictable results and is strongly discouraged.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the COUNT
argument of `MPI_Status_set_elements_x` only for Fortran 90. FORTRAN 77
users may use the non-portable syntax
    where MPI_COUNT_KIND is a constant defined in mpif.h and gives the length of the declared integer in bytes.
