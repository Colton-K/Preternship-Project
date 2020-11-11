---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Info_delete
---

NAME
====

**MPI_Info_delete** - Deletes a key/value pair from *info*.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Info_delete(MPI_Info info, const char *key)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_INFO_DELETE(INFO, KEY, IERROR)
    	INTEGER		INFO, IERROR
    	CHARACTER*(*)	KEY

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Info_delete(info, key, ierror)
    	TYPE(MPI_Info), INTENT(IN) :: info
    	CHARACTER(LEN=*), INTENT(IN) :: key
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT/OUTPUT PARAMETER
======================

info

:   Info object (handle).

INPUT PARAMETER
===============

key

:   Key (string).

OUTPUT PARAMETER
================

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

MPI_Info_delete deletes a (key,value) pair from *info*. If *key* is not
defined in *info*, the call raises an error of class MPI_ERR_INFO_NOKEY.

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
MPI_Info_dup\
MPI_Info_free\
MPI_Info_get\
MPI_Info_set\
