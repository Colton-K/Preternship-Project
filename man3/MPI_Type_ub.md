# Name

`MPI_Type_ub` - Returns the upper bound of a datatype -- use of this
routine is deprecated.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Type_ub(MPI_Datatype datatype, MPI_Aint *displacement)
```

## Fortran Syntax

```fortran
INCLUDE 'mpif.h'

MPI_TYPE_UB(DATATYPE, DISPLACEMENT, IERROR)
    INTEGER    DATATYPE, DISPLACEMENT, IERROR
```


# Input Parameter

* `datatype` : Datatype (handle).

# Output Parameters

* `displacement` : Displacement of upper bound from origin, in bytes (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

Note that use of this routine is `deprecated` as of MPI-2. Please use
`MPI_Type_get_extent` instead.
`MPI_Type_ub` returns the upper bound of a data type. This will differ
from zero if the type was constructed using `MPI_UB`. The upper bound will
take into account any alignment considerations.
The "pseudo-datatypes," `MPI_LB` and `MPI_UB`, can be used, respectively,
to mark the upper bound (or the lower bound) of a datatype. These
pseudo-datatypes occupy no space (extent `(MPI_LB`) = extent `(MPI_UB`) =0.
They do not affect the size or count of a datatype, and do not affect
the context of a message created with this datatype. However, they do
affect the definition of the extent of a `datatype` and, therefore, affect
the outcome of a replication of this `datatype` by a `datatype` constructor.
In general, if
        Typemap = {(type(0), disp(0)), ..., (type(n-1), disp(n-1))}
then the lower bound of Typemap is defined to be
                      (min(j) disp(j)                          if no entry has
        lb(Typemap) = (                                        basic type lb
                      (min(j) {disp(j) such that type(j) = lb} otherwise
Similarly, the upper bound of Typemap is defined to be
                      (max(j) disp(j) + sizeof(type(j) = lb}   if no entry has
        ub(Typemap) = (                                        basic type ub
                      (max(j) {disp(j) such that type(j) = ub} otherwise
Then
        extent(Typemap) = ub(Typemap) - lb(Typemap)
If type(i) requires alignment to a byte address that is a multiple of
k(i), then e is the least nonnegative increment needed to round
extent(Typemap) to the next multiple of max(i) k(i).

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

[`MPI_Type_get_extent(3)`](./?file=MPI_Type_get_extent.md)
