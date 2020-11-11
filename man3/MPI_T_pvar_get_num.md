# Name

`MPI_T_pvar_get_num` - Query the number of performance variables

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_T_pvar_get_num(int *num_pvar)
```


# Output Parameters

* `num_pvar` : Current number of performance variables.

# Description

`MPI_T_pvar_get_num` can be used to query the current number of
performance variables. The number of performance variables may increase
throughout the exection of the process but will never decrease.

# Errors

`MPI_T_pvar_get_num(`) will fail if:
* `[MPI_T_ERR_NOT_INITIALIZED]` : The MPI Tools interface not initialized
:   The MPI Tools interface not initialized
