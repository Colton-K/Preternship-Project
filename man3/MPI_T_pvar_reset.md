# Name

`MPI_T_pvar_reset` - Reset the value of a performance variable

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_T_pvar_reset(MPI_T_pvar_session session, MPI_T_pvar_handle handle)
```


# Input Parameters

* `session` : Performance experiment session.
* `handle` : Performance variable handle or MPI_T_PVAR_ALL_HANDLES.

# Description

`MPI_T_pvar_reset` sets the performance variable specified by the handle
in `handle` to its initial value. The special value
`MPI_T_PVAR_ALL_HANDLES` can be passed in `handle` to reset all
read-write handles in the `session` specified in `session`.

# Errors

`MPI_T_pvar_reset(`) will fail if:
* `[MPI_T_ERR_NOT_INITIALIZED]` : The MPI Tools interface not initialized
:   The MPI Tools interface not initialized
* `[MPI_T_ERR_INVALID_HANDLE]` : The handle is invalid
:   The `handle` is invalid
* `[MPI_T_ERR_INVALID_SESSION]` : Session argument is not a valid session
:   Session argument is not a valid session
* `[MPI_T_ERR_PVAR_NO_WRITE]` : Variable cannot be reset
:   Variable cannot be reset

# See Also

[`MPI_T_pvar_handle_alloc(3)`](./?file=MPI_T_pvar_handle_alloc.md)
[`MPI_T_pvar_get_info(3)`](./?file=MPI_T_pvar_get_info.md)
[`MPI_T_pvar_session_create(3)`](./?file=MPI_T_pvar_session_create.md)
[`MPI_T_pvar_write(3)`](./?file=MPI_T_pvar_write.md)
