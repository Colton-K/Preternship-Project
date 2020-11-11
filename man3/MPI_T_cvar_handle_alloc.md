# Name

`MPI_T_cvar_handle_alloc`, `MPI_T_cvar_handle_free` -
Allocate/free contol variable handles

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_T_cvar_handle_alloc(int cvar_index, void *obj_handle,

                            MPI_T_cvar_handle *handle, int *count)

int MPI_T_cvar_handle_free(MPI_T_cvar_handle *handle)
```


# Description

`MPI_T_cvar_handle_alloc` binds the control variable specified in
`cvar_index` to the MPI object specified in `obj_handle`. If
`MPI_T_cvar_get_info` returns `MPI_T_BIND_NO_OBJECT` as the binding of the
variable the `obj_handle` argument is ignored. The number of values
represented by this control variable is returned in the count
parameter. If the control variable represents a string then `count` will
be the maximum length of the string.
`MPI_T_cvar_handle_free` frees a handle allocated by
`MPI_T_cvar_handle_alloc` and sets the `handle` argument to

# Notes

Open MPI does not currently support binding MPI objects to control
variables so the `obj_handle` argument is always ignored.

# Errors

`MPI_T_cvar_handle_alloc(`) will fail if:
* `[MPI_T_ERR_NOT_INITIALIZED]` : The MPI Tools interface not initialized
:   The MPI Tools interface not initialized
* `[MPI_T_ERR_INVALID_INDEX]` : The control variable index is invalid
:   The control variable index is invalid
* `[MPI_T_ERR_OUT_OF_HANDLES]` : No more handles available
:   No more handles available
* `MPI_T_cvar_handle_free() will fail if:` : 
* `[MPI_T_ERR_NOT_INITIALIZED]` : The MPI Tools interface not initialized
:   The MPI Tools interface not initialized
* `[MPI_T_ERR_INVALID_HANDLE]` : The handle is invalid
:   The handle is invalid

# See Also

[`MPI_T_cvar_get_info(3)`](./?file=MPI_T_cvar_get_info.md)
