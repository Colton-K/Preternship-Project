---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Info_get
---

NAME
====

**MPI_Info_get** - Retrieves the value associated with a key in an info
object.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Info_get(MPI_Info info, const char *key, int valuelen, char *value, int *flag)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_INFO_GET(INFO, KEY, VALUELEN, VALUE, FLAG, IERROR)
    	INTEGER	INFO, VALUELEN, IERROR
    	CHARACTER*(*) KEY, VALUE
    	LOGICAL FLAG

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Info_get(info, key, valuelen, value, flag, ierror)
    	TYPE(MPI_Info), INTENT(IN) :: info
    	CHARACTER(LEN=*), INTENT(IN) :: key
    	INTEGER, INTENT(IN) :: valuelen
    	CHARACTER(LEN=valuelen), INTENT(OUT) :: value
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

```{=html}
<!-- -->
```

valuelen

:   Length of value arg (integer).

OUTPUT PARAMETER
================

value

:   Value (string).

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

MPI_Info_get retrieves the value associated with *key* in a previous
call to MPI_Info_set. If such a key exists, it sets *flag* to true and
returns the value in *value*; otherwise it sets *flag* to false and
leaves *value* unchanged. *valuelen* is the number of characters
available in value. If it is less than the actual size of the value, the
returned value is truncated. In C, *valuelen* should be one less than
the amount of allocated space to allow for the null terminator.

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

MPI_Info_create\
MPI_Info_delete\
MPI_Info_dup\
MPI_Info_free\
MPI_Info_get_valuelen\
MPI_Info_get_nkeys\
MPI_Info_get_nthkey\
MPI_Info_set\
