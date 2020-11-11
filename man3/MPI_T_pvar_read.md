# Name

`MPI_T_pvar_read` - Read the value of a performance variable

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_T_pvar_read(MPI_T_pvar_session session, MPI_T_pvar_handle handle, const void *buf)
```


# Input Parameters

* `session` : Performance experiment session.
* `handle` : Performance variable handle.
* `buf` : Initial address of storage location for variable value.

# Description

`MPI_T_pvar_read` queries the value of a performance variable identified
by the `handle` specified in ``handle`` in the `session` specified in
session. The result is stored in the buffer pointed to by buf. The
caller must ensure that the buffer pointed to by `buf` is large enough
to hold the entire value of the performance variable.

# Errors

`MPI_T_pvar_read(`) will fail if:
* `[MPI_T_ERR_NOT_INITIALIZED]` : The MPI Tools interface not initialized
:   The MPI Tools interface not initialized
* `[MPI_T_ERR_INVALID_HANDLE]` : The handle is invalid or not associated with the session
:   The `handle` is invalid or not associated with the session
* `[MPI_T_ERR_INVALID_SESSION]` : Session argument is not a valid session
:   Session argument is not a valid session

# See Also

[`MPI_T_pvar_handle_alloc(3)`](./?file=MPI_T_pvar_handle_alloc.md)
[`MPI_T_pvar_get_info(3)`](./?file=MPI_T_pvar_get_info.md)
[`MPI_T_pvar_session_create(3)`](./?file=MPI_T_pvar_session_create.md)
