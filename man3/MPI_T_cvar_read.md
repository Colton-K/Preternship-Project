# Name

`MPI_T_cvar_read` - Read the value of a control variable

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_T_cvar_read(MPI_T_cvar_handle handle, const void *buf)
```


# Input Parameters

* `handle` : Handle of the control variable to be read.
* `buf` : Initial address of storage location for variable value.

# Description

`MPI_T_cvar_read` reads the value of the control variable identified by
the `handle` specified in ``handle`` and stores the value in the buffer
pointed to by buf. The caller must ensure that the buffer pointed to
by `buf` is large enough to hold the entire value of the control
variable.

# Errors

`MPI_T_cvar_read(`) will fail if:
* `[MPI_T_ERR_NOT_INITIALIZED]` : The MPI Tools interface not initialized
:   The MPI Tools interface not initialized
* `[MPI_T_ERR_INVALID_HANDLE]` : The handle is invalid
:   The `handle` is invalid

# See Also

[`MPI_T_cvar_handle_alloc(3)`](./?file=MPI_T_cvar_handle_alloc.md)
[`MPI_T_cvar_get_info(3)`](./?file=MPI_T_cvar_get_info.md)
