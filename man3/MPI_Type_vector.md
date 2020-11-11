# Name

`MPI_Type_vector` - Creates a vector (strided) datatype.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Type_vector(int count, int blocklength, int stride,

    MPI_Datatype oldtype, MPI_Datatype *newtype)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_TYPE_VECTOR(COUNT, BLOCKLENGTH, STRIDE, OLDTYPE, NEWTYPE,
        IERROR)
    INTEGER    COUNT, BLOCKLENGTH, STRIDE, OLDTYPE
    INTEGER    NEWTYPE, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Type_vector(count, blocklength, stride, oldtype, newtype, ierror)
    INTEGER, INTENT(IN) :: count, blocklength, stride
    TYPE(MPI_Datatype), INTENT(IN) :: oldtype
    TYPE(MPI_Datatype), INTENT(OUT) :: newtype
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `count` : Number of blocks (nonnegative integer).
* `blocklength` : Number of elements in each block (nonnegative integer).
* `stride` : Number of elements between start of each block (integer).
* `oldtype` : Old datatype (handle).

# Output Parameters

* `newtype` : New datatype (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

The function `MPI_Type_vector` is a general constructor that allows
replication of a datatype into locations that consist of equally spaced
blocks. Each block is obtained by concatenating the same number of
copies of the old datatype. The spacing between blocks is a multiple of
the extent of the old datatype.
Example 1: Assume, again, that `oldtype` has type map {(double, 0),
(char, 8)}, with extent 16. A call to `MPI_Type_vector(2`, 3, 4, oldtype,
newtype) will create the datatype with type map
        {(double, 0), (char, 8), (double, 16), (char, 24),
        (double, 32), (char, 40),
        (double, 64), (char, 72),
        (double, 80), (char, 88), (double, 96), (char, 104)}
That is, two blocks with three copies each of the old type, with a
stride of 4 elements (4 x 16 bytes) between the blocks.
Example 2: A call to `MPI_Type_vector(3`, 1, -2, oldtype, newtype)
will create the datatype
        {(double, 0), (char, 8), (double, -32), (char, -24),
        (double, -64), (char, -56)}
In general, assume that `oldtype` has type map
        {(type(0), disp(0)), ..., (type(n-1), disp(n-1))},
with extent ex. Let bl be the blocklength. The newly created datatype
has a type map with `count` x bl x n entries:
        {(type(0), disp(0)), ..., (type(n-1), disp(n-1)),
        (type(0), disp(0) + ex), ..., (type(n-1), disp(n-1) + ex), ...,
        (type(0), disp(0) + (bl -1)  ex),...,
        (type(n-1), disp(n-1) + (bl -1) ex),
        (type(0), disp(0) + `stride`  ex),..., (type(n-1),
        disp(n-1) + `stride`  ex), ...,
        (type(0), disp(0) + (stride + bl - 1)  ex), ...,
        (type(n-1), disp(n-1) + (stride + bl -1)  ex), ...,
        (type(0), disp(0) + `stride`  (count -1)  ex), ...,
        (type(n-1), disp(n-1) + `stride`  (count -1)  ex), ...,
        (type(0), disp(0) + (stride  (count -1) + bl -1)  ex), ...,
        (type(n-1), disp(n-1) + (stride  (count -1) + bl -1)  ex)}
A call to `MPI_Type_contiguous(count`, oldtype, newtype) is equivalent to
a call to `MPI_Type_vector(count`, 1, 1, oldtype, newtype), or to a call
to `MPI_Type_vector(1`, count, n, oldtype, newtype), n arbitrary.

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
[`MPI_Type_hvector(3)`](./?file=MPI_Type_hvector.md)
