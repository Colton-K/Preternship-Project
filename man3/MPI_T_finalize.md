# Name

MPI_T_finalize  - Finalize the MPI tool information interface

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_T_finalize(void)
```


# Description

`MPI_T_finalize(`) finalizes the MPI tool information interface and must
be called the same number of times as `MPI_T_init_thread(`) by the end of
execution. Calls to MPI tool functions are allowed at any point in
execution as long as `MPI_T_init_thread(`) has been called at least once
and the number of calls to `MPI_T_init_thread(`) is greater than the
number of calls to `MPI_T_finalize()`. If at any point in execution the
number of calls to `MPI_T_finalize(`) equals the number of calls to
`MPI_T_init_thread(`) the MPI tool interface will no longer be available
until another call to `MPI_T_init_thread().`

# Notes

Before the end of execution the number of calls to `MPI_T_init_thread()`
and `MPI_T_finalize` must be the same.

# Errors

`MPI_T_finalize(`) will fail if:
* `[MPI_T_ERR_NOT_INITIALIZED]` : The MPI Tools interface not initialized
:   The MPI Tools interface not initialized

# See Also

[`MPI_T_init_thread(3)`](./?file=MPI_T_init_thread.md)
