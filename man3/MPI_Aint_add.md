# Name

`MPI_Aint_add`, `MPI_Aint_diff` - Portable functions for arithmetic
on `MPI_Aint` values.

# Syntax

## C Syntax

```c
#include <mpi.h>

MPI_Aint MPI_Aint_add(MPI_Aint base, MPI_Aint disp)

MPI_Aint MPI_Aint_diff(MPI_Aint addr1, MPI_Aint addr2)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

INTEGER(KIND=MPI_ADDRESS_KIND) MPI_AINT_ADD(BASE, DISP)

        INTEGER(KIND=MPI_ADDRESS_KIND) BASE, DISP

INTEGER(KIND=MPI_ADDRESS_KIND) MPI_AINT_DIFF(ADDR1, ADDR2)

        INTEGER(KIND=MPI_ADDRESS_KIND) ADDR1, ADDR2
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

INTEGER(KIND=MPI_ADDRESS_KIND) MPI_AINT_ADD(BASE, DISP)

        INTEGER(KIND=MPI_ADDRESS_KIND) BASE, DISP

INTEGER(KIND=MPI_ADDRESS_KIND) MPI_AINT_DIFF(ADDR1, ADDR2)

        INTEGER(KIND=MPI_ADDRESS_KIND) ADDR1, ADDR2
```


# Input Parameters

* `base` : Base address (integer).
* `disp` : Displacement (integer).
* `addr1` : Minuend address (integer).
* `addr2` : Subtrahend address (integer).

# Description

`MPI_Aint_add` produces a new `MPI_Aint` value that is equivalent to the
sum of the `base` and `disp` arguments, where `base` represents a base
address returned by a call to `MPI_Get_address` and `disp` represents
a signed integer displacement. The resulting address is valid only at
the process that generated base, and it must correspond to a location
in the same object referenced by base, as described in MPI-3.1 �
4.1.12. The addition is performed in a manner that results in the
correct `MPI_Aint` representation of the output address, as if the process
that originally produced `base` had called:
            MPI_Get_address ((char ) `base` + disp, &result);
`MPI_Aint_diff` produces a new `MPI_Aint` value that is equivalent to
the difference between `addr1` and `addr2` arguments, where `addr1` and
addr2 represent addresses returned by calls to `MPI_Get_address.`
The resulting address is valid only at the process that generated
`addr1` and addr2, and `addr1` and `addr2` must correspond to
locations in the same object in the same process, as described in
MPI-3.1 � 4.1.12. The difference is calculated in a manner that results
in the signed difference from `addr1` to addr2, as if the process that
originally produced the addresses had called (char ) `addr1` - (char
) `addr2` on the addresses initially passed to `MPI_Get_address.`

# See Also

[`MPI_Get_address(3)`](./?file=MPI_Get_address.md)
