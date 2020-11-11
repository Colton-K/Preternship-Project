---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Comm_split_type
---

NAME
====

**MPI_Comm_split_type ** - Creates new communicators based on colors and
keys.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Comm_split_type(MPI_Comm comm, int split_type, int key,
    	MPI_Info info, MPI_Comm *newcomm)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_COMM_SPLIT_TYPE(COMM, SPLIT_TYPE, KEY, INFO, NEWCOMM, IERROR)
    	INTEGER	COMM, SPLIT_TYPE, KEY, INFO, NEWCOMM, IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Comm_split_type(comm, split_type, key, info, newcomm, ierror)
    	TYPE(MPI_Comm), INTENT(IN) :: comm
    	INTEGER, INTENT(IN) :: split_type, key
    	TYPE(MPI_Info), INTENT(IN) :: info
    	TYPE(MPI_Comm), INTENT(OUT) :: newcomm
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
================

comm

:   Communicator (handle).

split_type

:   Type of processes to be grouped together (integer).

key

:   Control of rank assignment (integer).

info

:   Info argument (handle).

OUTPUT PARAMETERS
=================

newcomm

:   New communicator (handle).

```{=html}
<!-- -->
```

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

This function partitions the group associated with *comm* into disjoint
subgroups, based on the type specied by *split_type*. Each subgroup
contains all processes of the same type. Within each subgroup, the
processes are ranked in the order defined by the value of the argument
*key*, with ties broken according to their rank in the old group. A new
communicator is created for each subgroup and returned in newcomm. This
is a collective call; all processes must provide the same *split_type*,
but each process is permitted to provide different values for key. An
exception to this rule is that a process may supply the type value
MPI_UNDEFINED, in which case newcomm returns MPI_COMM_NULL.

SPLIT TYPES
===========

MPI_COMM_TYPE_SHARED

:   This type splits the communicator into subcommunicators, each of
    which can create a shared memory region.

```{=html}
<!-- -->
```

OMPI_COMM_TYPE_NODE

:   Synonym for MPI_COMM_TYPE_SHARED.

```{=html}
<!-- -->
```

OMPI_COMM_TYPE_HWTHREAD

:   This type splits the communicator into subcommunicators, each of
    which belongs to the same hardware thread.

```{=html}
<!-- -->
```

OMPI_COMM_TYPE_CORE

:   This type splits the communicator into subcommunicators, each of
    which belongs to the same core/processing unit.

```{=html}
<!-- -->
```

OMPI_COMM_TYPE_L1CACHE

:   This type splits the communicator into subcommunicators, each of
    which belongs to the same L1 cache.

```{=html}
<!-- -->
```

OMPI_COMM_TYPE_L2CACHE

:   This type splits the communicator into subcommunicators, each of
    which belongs to the same L2 cache.

```{=html}
<!-- -->
```

OMPI_COMM_TYPE_L3CACHE

:   This type splits the communicator into subcommunicators, each of
    which belongs to the same L3 cache.

```{=html}
<!-- -->
```

OMPI_COMM_TYPE_SOCKET

:   This type splits the communicator into subcommunicators, each of
    which belongs to the same socket.

```{=html}
<!-- -->
```

OMPI_COMM_TYPE_NUMA

:   This type splits the communicator into subcommunicators, each of
    which belongs to the same NUMA-node.

```{=html}
<!-- -->
```

OMPI_COMM_TYPE_BOARD

:   This type splits the communicator into subcommunicators, each of
    which belongs to the same board.

```{=html}
<!-- -->
```

OMPI_COMM_TYPE_HOST

:   This type splits the communicator into subcommunicators, each of
    which belongs to the same host.

```{=html}
<!-- -->
```

OMPI_COMM_TYPE_CU

:   This type splits the communicator into subcommunicators, each of
    which belongs to the same computational unit.

```{=html}
<!-- -->
```

OMPI_COMM_TYPE_CLUSTER

:   This type splits the communicator into subcommunicators, each of
    which belongs to the same cluster.

NOTES
=====

The communicator keys denoted with an *OMPI\_* prefix instead of an
*MPI\_* prefix are specific to Open MPI, and are not part of the MPI
standard. Their use should be protected by the *OPEN_MPI* C preprocessor
macro.

ERRORS
======

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
MPI_Comm_set_errhandler; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

SEE ALSO
========

MPI_Comm_create\
MPI_Intercomm_create\
MPI_Comm_dup\
MPI_Comm_free\
MPI_Comm_split
