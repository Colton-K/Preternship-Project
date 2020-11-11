# Name

`MPI_Type_create_subarray` - Creates a data type describing an
n-dimensional subarray of an n-dimensional array.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Type_create_subarray(int ndims, const int array_of_sizes[], const int array_of_subsizes[], const int array_of_starts[], int order, MPI_Datatype oldtype, MPI_Datatype *newtype)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_TYPE_CREATE_SUBARRAY(NDIMS, ARRAY_OF_SIZES, ARRAY_OF_SUBSIZES,
    ARRAY_OF_STARTS, ORDER, OLDTYPE, NEWTYPE, IERROR)
    INTEGER    NDIMS, ARRAY_OF_SIZES(*), ARRAY_OF_SUBSIZES(*),
    ARRAY_OF_STARTS(*), ORDER, OLDTYPE, NEWTYPE, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Type_create_subarray(ndims, array_of_sizes, array_of_subsizes,
        array_of_starts, order, oldtype, newtype, ierror)
    INTEGER, INTENT(IN) :: ndims, array_of_sizes(ndims),
    array_of_subsizes(ndims), array_of_starts(ndims), order
    TYPE(MPI_Datatype), INTENT(IN) :: oldtype
    TYPE(MPI_Datatype), INTENT(OUT) :: newtype
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `ndims` : Number of array dimensions (positive integer).
* `array_of_sizes` : Number of elements of type *oldtype* in each dimension of the full
array (array of positive integers).
* `array_of_subsizes` : Number of elements of type *oldtype* in each dimension of the
subarray (array of positive integers).
* `array_of_starts` : Starting coordinates of the subarray in each dimension (array of
nonnegative integers).
* `order` : Array storage order flag (state).
* `oldtype` : Array element data type (handle).
```


# Output Parameters

* `newtype` : New data type (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

The subarray type constructor creates an MPI data type describing an
n-dimensional subarray of an n-dimensional array. The subarray may
be situated anywhere within the full array, and may be of any nonzero
size up to the size of the larger array as long as it is confined within
this array. This type constructor facilitates creating file types to
access arrays distributed in blocks among processes to a single file
that contains the global array.
This type constructor can handle arrays with an arbitrary number of
dimensions and works for both C- and Fortran-ordered matrices (that is,
row-major or column-major). Note that a C program may use Fortran order
and a Fortran program may use C order.
The `ndims` parameter specifies the number of dimensions in the full
data array and gives the number of elements in `array_of_sizes,`
`array_of_subsizes`, and `array_of_starts.`
The number of elements of type `oldtype` in each dimension of the
n-dimensional array and the requested subarray are specified by
`array_of_sizes` and `array_of_subsizes`, respectively. For any
dimension i, it is erroneous to specify `array_of_subsizes[i]` < 1
or `array_of_subsizes[i]` > array of sizes[i].
The `array_of_starts` contains the starting coordinates of each
dimension of the subarray. Arrays are assumed to be indexed starting
from zero. For any dimension i, it is erroneous to specify
    array_of_starts[i] < 0
or
    array_of_starts[i] > (array_of_sizes[i] - array_of_subsizes[i]).
The ``order`` argument specifies the storage `order` for the subarray as
well as the full array. It must be set to one of the following:
- `MPI_ORDER_C`: The ordering used by C arrays, (that is, row-major
order)
- `MPI_ORDER_FORTRAN`: The ordering used by Fortran arrays, (that is,
column-major order)
A ndims-dimensional subarray (newtype) with no extra padding can be
defined by the function Subarray() as follows:
       `newtype` = Subarray(ndims, {size , size ,..., size       },
                                      0      1           ndims-1
                 {subsize , subsize , ..., subsize       },
                         0         1               ndims-1
                 {start , start , ..., start       }, oldtype)
                       0       1             ndims-1
Let the typemap of `oldtype` have the form:
       {(type , disp ), (type , disp ), ..., (type   , disp   )}
             0      0        1      1              n-1      n-1
where typei is a predefined MPI data type, and let `ex` be the extent
of oldtype.
The Subarray() function is defined recursively in three equations on
page 72 of the MPI-2 standard.
For an example use of `MPI_Type_create_subarray` in the context of I/O,
see Section 9.9.2 of the MPI-2 standard.

# Notes

In a Fortran program with arrays indexed starting from 1, if the
starting coordinate of a particular dimension of the subarray is n,
then the entry in array of starts for that dimension is n-1.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
