---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Info_dup
---

NAME
====

**MPI_Info_dup** - Duplicates an info object.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Info_dup(MPI_Info info, MPI_Info *newinfo)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_INFO_DUP(INFO, NEWINFO, IERROR)
    	INTEGER		INFO, NEWINFO, IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Info_dup(info, newinfo, ierror)
    	TYPE(MPI_Info), INTENT(IN) :: info
    	TYPE(MPI_Info), INTENT(OUT) :: newinfo
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETER
===============

info

:   Info object (handle).

OUTPUT PARAMETERS
=================

newinfo

:   Info object (handle).

```{=html}
<!-- -->
```

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

MPI_Info_dup duplicates an existing info object, creating a new object,
with the same (key,value) pairs and the same ordering of keys.

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
MPI_Info_free\
MPI_Info_get\
MPI_Info_set\
