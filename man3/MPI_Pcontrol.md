# Name

`MPI_Pcontrol` - Controls profiling.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Pcontrol(const int level, ... )
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_PCONTROL(LEVEL)
    INTEGER    LEVEL
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Pcontrol(level)
    INTEGER, INTENT(IN) :: level
```


# Input Parameter

* `level` : Profiling level.

# Description

MPI libraries themselves make no use of this routine; they simply return
immediately to the user code. However the presence of calls to this
routine allows a profiling package to be explicitly called by the user.
Since MPI has no control of the implementation of the profiling code, we
are unable to specify precisely the semantics that will be provided by
calls to `MPI_Pcontrol`. This vagueness extends to the number of arguments
to the function, and their datatypes.
However to provide some `level` of portability of user codes to different
profiling libraries, we request the following meanings for certain
values of level:
* ` o` : level==0 Profiling is disabled.
:   level==0 Profiling is disabled.
* ` o` : level==1 Profiling is enabled at a normal default level of detail.
:   `level`==1 Profiling is enabled at a normal default `level` of detail.
* ` o` : level==2 Profile buffers are flushed. (This may be a no-op in some
:   level==2 Profile buffers are flushed. (This may be a no-op in some
    profilers).
* ` o` : All other values of level have profile library-defined effects and
:   All other values of `level` have profile library-defined effects and
    additional arguments.
We also request that the default state after `MPI_Init` has been called is
for profiling to be enabled at the normal default `level` (i.e., as if
`MPI_Pcontrol` had just been called with the argument 1). This allows
users to link with a profiling library and obtain profile output without
having to modify their source code at all.
The provision of `MPI_Pcontrol` as a no-op in the standard MPI library
allows users to modify their source code to obtain more detailed
profiling information, but still be able to link exactly the same code
against the standard MPI library.

# Notes

This routine provides a common interface for profiling control. The
interpretation of `level` and any other arguments is left to the profiling
library.
This function does not return an error value. Consequently, the result
of calling it before `MPI_Init` or after `MPI_Finalize` is undefined.
