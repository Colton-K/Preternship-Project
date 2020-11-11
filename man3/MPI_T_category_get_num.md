# Name

`MPI_T_category_get_num` - Query the number of categories

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_T_category_get_num(int *num_cat)
```


# Output Parameters

* `num_cat` : Current number of categories

# Description

`MPI_T_category_get_num` can be used to query the current number of
categories.

# Errors

`MPI_T_category_get_num(`) will fail if:
* `[MPI_T_ERR_NOT_INITIALIZED]` : The MPI Tools interface not initialized
:   The MPI Tools interface not initialized
