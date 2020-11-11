# Name

`MPI_T_category_get_categories` - Query which categories are in a
category

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_T_category_get_categories(int cat_index, int len, int indices[])
```


# Input Parameters

* `cat_index` : Index of the category to be queried.
* `len` : The length of the indices array.

# Output Parameters

* `indices` : An integer array of size len, indicating category indices.

# Description

`MPI_T_category_get_categories` can be used to query which other
categories are in a category.

# Errors

`MPI_T_category_get_categories(`) will fail if:
* `[MPI_T_ERR_NOT_INITIALIZED]` : The MPI Tools interface not initialized
:   The MPI Tools interface not initialized
* `[MPI_T_ERR_INVALID_INDEX]` : The category index is invalid
:   The category index is invalid
