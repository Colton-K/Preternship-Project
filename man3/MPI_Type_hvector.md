# Name

`MPI_Type_hvector` - Creates a vector (strided) datatype with offset
in bytes -- use of this routine is deprecated.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Type_hvector(int count, int blocklength, MPI_Aint stride,

    MPI_Datatype oldtype, MPI_Datatype *newtype)
```

## Fortran Syntax

```fortran
INCLUDE 'mpif.h'

MPI_TYPE_HVECTOR(COUNT, BLOCKLENGTH, STRIDE, OLDTYPE, NEWTYPE,
        IERROR)
    INTEGER    COUNT, BLOCKLENGTH, STRIDE, OLDTYPE
    INTEGER    NEWTYPE, IERROR
```


# Input Parameters

* `count` : Number of blocks (nonnegative integer).
* `blocklength` : Number of elements in each block (nonnegative integer).
* `stride` : Number of bytes between start of each block (integer).
* `oldtype` : Old datatype (handle).

# Output Parameters

* `newtype` : New datatype (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

Note that use of this routine is `deprecated` as of MPI-2. Use
`MPI_Type_create_hvector` instead.
The function `MPI_Type_hvector` is identical to `MPI_Type_vector`, except
that `stride` is given in bytes, rather than in elements. The use for both
types of vector constructors is illustrated in the examples in Section
3.12.7 of the MPI-1 Standard.
Assume that `oldtype` has type map
        {(type(0), disp(0)), ..., (type(n-1), disp(n-1))}
with extent ex. Let bl be the blocklength. The newly created datatype
has a type map with `count`  bl  n entries:
      {(type(0), disp(0)), ..., (type(n-1), disp(n-1)),
      (type(0), disp(0) + ex), ..., (type(n-1), disp(n-1) + ex),
      ..., (type(0), disp(0) + (bl -1)  ex),...,(type(n-1),
      disp(n-1) + (bl -1)  ex), (type(0), disp(0) + stride),
      ...,(type(n-1), disp(n-1) + stride), ..., (type(0),
      disp(0) + `stride` + (bl - 1)  ex), ..., (type(n-1),
      disp(n-1) + `stride` + (bl -1)  ex), ..., (type(0),
      disp(0) + `stride`  (count -1)), ...,(type(n-1),
      disp(n-1) + `stride`  (count -1)), ..., (type(0),
      disp(0) + `stride`  (count -1) + (bl -1)  ex), ...,
      (type(n-1), disp(n-1) + `stride`  (count -1) + (bl -1)  ex)}

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

[`MPI_Type_create_hvector(3)`](./?file=MPI_Type_create_hvector.md)
[`MPI_Type_vector(3)`](./?file=MPI_Type_vector.md)
