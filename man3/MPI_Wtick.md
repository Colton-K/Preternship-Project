# Name

`MPI_Wtick` - Returns the resolution of `MPI_Wtime.`

# Syntax

## C Syntax

```c
#include <mpi.h>

double MPI_Wtick()
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

DOUBLE PRECISION MPI_WTICK()
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

DOUBLE PRECISION MPI_WTICK()
```


# Return Value

Time in seconds of resolution of `MPI_Wtime.`

# Description

`MPI_Wtick` returns the resolution of `MPI_Wtime` in seconds. That is, it
returns, as a double-precision value, the number of seconds between
successive clock ticks. For example, if the clock is implemented by the
hardware as a counter that is incremented every millisecond, the value
returned by `MPI_Wtick` should be 10^-3.

# Note

This function does not return an error value. Consequently, the result
of calling it before `MPI_Init` or after `MPI_Finalize` is undefined.

# See Also

[`MPI_Wtime(3)`](./?file=MPI_Wtime.md)
