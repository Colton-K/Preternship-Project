---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Errhandler_set
---

NAME
====

**MPI_Errhandler_set ** - Sets the error handler for a communicator \--
use of this routine is deprecated.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Errhandler_set(MPI_Comm comm, MPI_Errhandler errhandler)

Fortran Syntax
==============

    INCLUDE 'mpif.h'
    MPI_ERRHANDLER_SET(COMM, ERRHANDLER, IERROR)
    	INTEGER	COMM, ERRHANDLER, IERROR

INPUT PARAMETERS
================

comm

:   Communicator to set the error handler for (handle).

errhandler

:   New MPI error handler for communicator (handle).

OUTPUT PARAMETER
================

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

Note that use of this routine is *deprecated* as of MPI-2. Please use
MPI_Comm_set_errhandler instead.

Associates the new error handler errhandler with communicator comm at
the calling process. Note that an error handler is always associated
with the communicator.

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
