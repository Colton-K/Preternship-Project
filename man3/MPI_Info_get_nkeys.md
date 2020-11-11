---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Info_get_nkeys
---

NAME
====

**MPI_Info_get_nkeys** - Gets the number of keys currently defined in an
info object.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Info_get_nkeys(MPI_Info info, int *nkeys)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_INFO_GET_NKEYS(INFO, NKEYS, IERROR)
    	INTEGER		INFO, NKEYS, IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Info_get_nkeys(info, nkeys, ierror)
    	TYPE(MPI_Info), INTENT(IN) :: info
    	INTEGER, INTENT(OUT) :: nkeys
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETER
===============

info

:   Info object (handle).

OUTPUT PARAMETERS
=================

nkeys

:   Number of defined keys (integer).

```{=html}
<!-- -->
```

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

MPI_Info_get_nkeys returns the number of currently defined keys in
*info*.

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
MPI_Info_get_nthkey\
MPI_Info_get_valuelen\
