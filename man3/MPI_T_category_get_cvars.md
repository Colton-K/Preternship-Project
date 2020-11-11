# Name

`MPI_T_category_get_cvars` - Query which control variables are in a
category

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_T_category_get_cvars(int cat_index, int len, int indices[])
```


# Input Parameters

* `cat_index` : Index of the category to be queried.
* `len` : The length of the indices array.

# Output Parameters

* `indices` : An integer array of size len, indicating control variable indices.

# Description

`MPI_T_category_get_cvars` can be used to query which control variables
are contained in a particular category.

# Errors

`MPI_T_category_get_cvars(`) will fail if:
* `[MPI_T_ERR_NOT_INITIALIZED]` : The MPI Tools interface not initialized
:   The MPI Tools interface not initialized
* `[MPI_T_ERR_INVALID_INDEX]` : The category index is invalid
:   The category index is invalid
