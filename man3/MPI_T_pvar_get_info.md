# Name

`MPI_T_pvar_get_info` - Query information from a performance variable

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_T_pvar_get_info(int pvar_index, char *name, int *name_len,

                        int *verbosity, int *var_class, MPI_Datatype *datatype, MPI_T_enum *enumtype,
                        char *desc, int *desc_len, int *bind, int *readonly, int *continuous,
                        int *atomic)
```


# Input Parameters

* `pvar_index` : Index of the performance variable to be queried.

# Input/Output Parameters

* `name_len` : Length of the string and/or buffer for name.
* `desc_len` : Length of the string and/or buffer for desc.

# Output Parameters

* `name` : Buffer to return the string containing the name of the performance
variable.
* `verbosity` : Verbosity level of this variable.
* `var_class` : Class of performance variable.
* `datatype` : MPI datatype of the information stored in the performance variable.
* `enumtype` : Optional descriptor for enumeration information.
* `desc` : Buffer to return the string containing the description of the
performance variable.
* `bind` : Type of MPI object to which this variable must be bound.
* `readonly` : Flag indicating whether the variable can be written/reset.
* `continuous` : Flag indicating whether the variable can be started and stopped or
is continuously active.
* `atomic` : Flag indicating whether the variable can be atomically read and
reset.
```


# Description

`MPI_T_pvar_get_info` can be used to query information from a performance
variable. The function returns the verbosity, class, datatype,
enumeration type, and binding of the queried control variable in the
arguments verbosity, `var_class`, datatype, enumtype, and bind
respectively. Flags indicating whether the variable is read-only,
continuous, or `atomic` are returns in readonly, continuous, and
`atomic` accordingly. See MPI-3 § 14.3.7 for more information. See the
man page for `MPI_T_cvar_get_info` for information on variable verbosity.

# Variable Class

Performance variables are categorized into classes which describe their
initial value, valid types, and behavior. The class returned in the
`var_class` parameter may be one of the following:
* `MPI_T_PVAR_CLASS_STATE` : Variable represents a set of discrete states that may be described
:   Variable represents a set of discrete states that may be described
    by an enumerator. Variables of this class must be represented by an
    MPI_INT. The starting value is the current state of the variable.
* `MPI_T_PVAR_CLASS_LEVEL` : Variable represents the current utilization level of a resource.
:   Variable represents the current utilization level of a resource.
    Variables of this class must be represented by an MPI_UNSIGNED,
    MPI_UNSIGNED_LONG, MPI_UNSIGNED_LONG_LONG, or MPI_DOUBLE. The
    starting value is the current utilization level of the resource.
* `MPI_T_PVAR_CLASS_SIZE` : Variable represents the fixed size of a resource. Variables of this
:   Variable represents the fixed size of a resource. Variables of this
    class are represented by an MPI_UNSIGNED, MPI_UNSIGNED_LONG,
    MPI_UNSIGNED_LONG_LONG, or MPI_DOUBLE. The starting value is the
    current size of the resource.
* `MPI_T_PVAR_CLASS_PERCENTAGE` : Variable represents the current precentage utilization level of a
:   Variable represents the current precentage utilization level of a
    resource. Variables of this class are represented by an MPI_DOUBLE.
    The starting value is the current percentage utilization of the
    resource.
* `MPI_T_PVAR_CLASS_HIGHWATERMARK` : Variable represents the high watermark of the utilization of a
:   Variable represents the high watermark of the utilization of a
    resource. Variables of this class are represented by an
    MPI_UNSIGNED, MPI_UNSIGNED_LONG, MPI_UNSIGNED_LONG_LONG, or
    MPI_DOUBLE. The starting value is the current utilization of the
    resource.
* `MPI_T_PVAR_CLASS_HIGHWATERMARK` : Variable represents the low watermark of the utilization of a
:   Variable represents the low watermark of the utilization of a
    resource. Variables of this class are represented by an
    MPI_UNSIGNED, MPI_UNSIGNED_LONG, MPI_UNSIGNED_LONG_LONG, or
    MPI_DOUBLE. The starting value is the current utilization of the
    resource.
* `MPI_T_PVAR_CLASS_COUNTER` : Variable represents a count of the number of occurrences of a
:   Variable represents a count of the number of occurrences of a
    specific event. Variables of this class are represented by an
    MPI_UNSIGNED, MPI_UNSIGNED_LONG, or MPI_UNSIGNED_LONG_LONG. The
    starting value is 0.
* `MPI_T_PVAR_CLASS_COUNTER` : Variable represents an aggregated value that represents a sum of
:   Variable represents an aggregated value that represents a sum of
    arguments processed during a specific event. Variables of this class
    are represented by an MPI_UNSIGNED, MPI_UNSIGNED_LONG,
    MPI_UNSIGNED_LONG_LONG, or MPI_DOUBLE. The starting value is 0.
* `MPI_T_PVAR_CLASS_TIMER` : Variable represents the aggregated time spent by the MPI
:   Variable represents the aggregated time spent by the MPI
    implementation while processing an event, type of event, or section
    of code. Variables of this class are represented by an MPI_UNSIGNED,
    MPI_UNSIGNED_LONG, MPI_UNSIGNED_LONG_LONG, or MPI_DOUBLE. If the
    variable is represented by an MPI_DOUBLE the units will be the same
    as those used by MPI_Wtime(). The starting value is 0.
* `MPI_T_PVAR_CLASS_GENERIC` : Variable does not fit into any other class. Can by represented by an
:   Variable does not fit into any other class. Can by represented by an
    type supported by the MPI tool information interface (see DATATYPE).
    Starting value is variable specific.
For more information see MPI-3 � 14.3.7.

# Datatype

The `datatype` returned by `MPI_T_pvar_get_info` is restricted to one of
the following datatypes: `MPI_INT`, `MPI_UNSIGNED`, `MPI_UNSIGNED_LONG,`
`MPI_UNSIGNED_LONG_LONG`, `MPI_COUNT`, `MPI_CHAR`, and `MPI_DOUBLE`. For more
information on datatypes in the MPI Tool information interface see MPI-3

# Binding

Performance variables may be bound to an MPI object. The binding
returned in the `bind` parameter may be one of the following:
* `MPI_T_BIND_NO_OBJECT` : No object
:   No object
* `MPI_T_BIND_MPI_COMM` : MPI communicator
:   MPI communicator
* `MPI_T_BIND_MPI_DATATYPE` : MPI datatype
:   MPI datatype
* `MPI_T_BIND_MPI_ERRHANDLER` : MPI error handler
:   MPI error handler
* `MPI_T_BIND_MPI_FILE` : MPI file handle
:   MPI file handle
* `MPI_T_BIND_MPI_GROUP` : MPI group
:   MPI group
* `MPI_T_BIND_MPI_OP` : MPI reduction operator
:   MPI reduction operator
* `MPI_T_BIND_MPI_REQUEST` : MPI request
:   MPI request
* `MPI_T_BIND_MPI_WIN` : MPI window for one-sided communication
:   MPI window for one-sided communication
* `MPI_T_BIND_MPI_MESSAGE` : MPI message object
:   MPI message object
* `MPI_T_BIND_MPI_INFO` : MPI info object
:   MPI info object
For more information see MPI-3 � 14.3.2.

# Notes

This MPI tool interface function returns two strings. This function
takes two argument for each string: a buffer to store the string, and a
length which must initially specify the size of the buffer. If the
length passed is n then this function will copy at most n - 1 characters
of the string into the corresponding buffer and set the length to the
number of characters copied - 1. If the length argument is NULL or the
value specified in the length is 0 the corresponding string buffer is
ignored and the string is not returned. For more information see MPI-3 �

# Errors

`MPI_T_pvar_get_info(`) will fail if:
* `[MPI_T_ERR_NOT_INITIALIZED]` : The MPI Tools interface not initialized
:   The MPI Tools interface not initialized
* `[MPI_T_ERR_INVALID_INDEX]` : The performance variable index is invalid
:   The performance variable index is invalid

# See Also

[`MPI_T_cvar_get_info(3)`](./?file=MPI_T_cvar_get_info.md)
