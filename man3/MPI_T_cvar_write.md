# Name

`MPI_T_cvar_write` - Write the value of a bound control variable

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_T_cvar_write(MPI_T_cvar_handle handle, const void *buf)
```


# Input Parameters

* `handle` : Handle of the control variable to be written.
* `buf` : Initial address of storage location for variable value.

# Description

`MPI_T_cvar_write` sets the value the control variable identified by the
handle specified in `handle` from the buffer provided in buf. The
caller must ensure that the buffer specified in `buf` is large enough to
hold the entire value of the control variable. If the variable has
global scope, any write call must be issued on all connected MPI
processes. For more information see MPI-3 � 14.3.6.

# Errors

`MPI_T_cvar_write(`) will fail if:
* `[MPI_T_ERR_NOT_INITIALIZED]` : The MPI Tools interface not initialized
:   The MPI Tools interface not initialized
* `[MPI_T_ERR_INVALID_HANDLE]` : The handle is invalid
:   The `handle` is invalid
* `[MPI_T_ERR_CVAR_SET_NOT_NOW]` : Variable cannot be set at this moment
:   Variable cannot be set at this moment
* `[MPI_T_ERR_CVAR_SET_NEVER]` : Variable cannot be set until end of execution
:   Variable cannot be set until end of execution

# See Also

[`MPI_T_cvar_handle_alloc(3)`](./?file=MPI_T_cvar_handle_alloc.md)
[`MPI_T_cvar_get_info(3)`](./?file=MPI_T_cvar_get_info.md)
