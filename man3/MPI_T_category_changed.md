# Name

`MPI_T_category_changed` - Get a timestamp for the categories

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_T_category_changed(int *stamp)
```


# Input Parameters

* `stamp` : A virtual time stamp to indicate the last change to the categories.

# Description

If two subsequent calls to this routine return the same timestamp, it is
guaranteed that no categories have been changed or added. If the
timestamp from the second call is higher than some categories have been
added or changed.

# Errors

`MPI_T_category_changed(`) will fail if:
* `[MPI_T_ERR_NOT_INITIALIZED]` : The MPI Tools interface not initialized
:   The MPI Tools interface not initialized
