---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Info_get_valuelen
---

NAME
====

**MPI_Info_get_valuelen** - Retrieves the length of the key value
associated with an info object.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Info_get_valuelen(MPI_Info info, const char *key,
    	int *valuelen, int *flag)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_INFO_GET_VALUELEN(INFO, KEY, VALUELEN, FLAG, IERROR)
    	INTEGER		INFO, VALUELEN, IERROR
    	LOGICAL		FLAG
    	CHARACTER*(*)	KEY

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Info_get_valuelen(info, key, valuelen, flag, ierror)
    	TYPE(MPI_Info), INTENT(IN) :: info
    	CHARACTER(LEN=*), INTENT(IN) :: key
    	INTEGER, INTENT(OUT) :: valuelen
    	LOGICAL, INTENT(OUT) :: flag
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
================

info

:   Info object (handle).

```{=html}
<!-- -->
```

key

:   Key (string).

OUTPUT PARAMETERS
=================

valuelen

:   Length of value arg (integer).

```{=html}
<!-- -->
```

flag

:   Returns true if key defined, false if not (boolean).

```{=html}
<!-- -->
```

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

MPI_Info_get_valuelen retrieves the length of the *value* associated
with *key*. If *key* is defined, *valuelen* is set to the length of its
associated value and *flag* is set to true. If *key* is not defined,
*valuelen* is not touched and *flag* is set to false. The length
returned in C does not include the end-of-string character.

If *key* is larger than MPI_MAX_INFO_KEY, the call is erroneous.

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

MPI_Info_get\
MPI_Info_get_nkeys\
MPI_Info_get_nthkey\
