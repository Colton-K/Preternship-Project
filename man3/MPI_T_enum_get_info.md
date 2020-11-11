# Name

`MPI_T_enum_get_info` - Query information about an enumerator

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_T_enum_get_info(MPI_T_enum enumtype, int *num, char *name, int *name_len)
```


# Input Parameters

* `enumtype` : Enumerator to be queried.

# Input/Output Parameters

* `name_len` : Length of the string and/or buffer for name.

# Output Parameters

* `num` : number of discrete values represented by this enumeration.
* `name` : Buffer to return the string containing the name of the category.

# Description

`MPI_T_enum_get_info` can be used to query information about an
enumerator. The function returns the number of discrete values
represented by this enumerator in the `num` parameter.

# Notes

This MPI tool interface function returns the `name` of the enumeration as
a string. This function takes two argument for the string: `name` which
specifies a buffer where the `name` of the should be stored, and
`name_len` which must initially specify the size of the buffer pointed
to by name. This function will copy at most `name_len` - 1 characters
of the `name` and sets ``name`_len` to the number of characters returned +
1. If `name_len` is NULL or the value specified in `name_len` is 0 the
``name`` buffer is ignored and the `name` of the enumeration is not
returned.

# Errors

`MPI_T_enum_get_info(`) will fail if:
* `[MPI_T_ERR_NOT_INITIALIZED]` : The MPI Tools interface not initialized
:   The MPI Tools interface not initialized
* `[MPI_T_ERR_INVALID_INDEX]` : The enumeration is invalid or has been deleted
:   The enumeration is invalid or has been deleted
