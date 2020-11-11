---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Errhandler_free
---

NAME
====

**MPI_Errhandler_free ** - Frees an MPI-style error handler.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Errhandler_free(MPI_Errhandler *errhandler)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_ERRHANDLER_FREE(ERRHANDLER, IERROR)
    	INTEGER	ERRHANDLER, IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Errhandler_free(errhandler, ierror)
    	TYPE(MPI_Errhandler), INTENT(INOUT) :: errhandler
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETER
===============

errhandler

:   MPI error handler (handle). Set to MPI_ERRHANDLER_NULL on exit.

OUTPUT PARAMETER
================

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

Marks the error handler associated with errhandler for deallocation and
sets errhandler to MPI_ERRHANDLER_NULL. The error handler will be
deallocated after all communicators associated with it have been
deallocated.

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

MPI_Comm_create_errhandler\
MPI_Comm_get_errhandler\
MPI_Comm_set_errhandler
