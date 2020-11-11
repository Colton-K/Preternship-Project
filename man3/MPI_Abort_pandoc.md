---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Abort
---

NAME
====

**MPI_Abort** - Terminates MPI execution environment.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Abort(MPI_Comm comm, int errorcode)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_ABORT(COMM, ERRORCODE, IERROR)
    	INTEGER		COMM, ERRORCODE, IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Abort(comm, errorcode, ierror)
    	TYPE(MPI_Comm), INTENT(IN) :: comm
    	INTEGER, INTENT(IN) :: errorcode
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
================

comm

:   Communicator of tasks to abort.

errorcode

:   Error code to return to invoking environment.

OUTPUT PARAMETER
================

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

This routine makes a \"best attempt\" to abort all tasks in the group of
comm. This function does not require that the invoking environment take
any action with the error code. However, a UNIX or POSIX environment
should handle this as a return errorcode from the main program or an
abort (errorcode).

The long-term goal of the Open MPI implementation is to terminate all
processes in all tasks that contain a process in *comm, and the error
code is not returned to the invoking environment. At the moment, this
isn\'t fully implemented and MPI_Abort will terminate the entire job.*

Note: All associated processes are sent a SIGTERM.

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
