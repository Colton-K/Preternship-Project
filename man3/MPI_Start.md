---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Start
---

NAME
====

**MPI_Start** - Initiates a communication using a persistent request
handle.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Start(MPI_Request *request)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_START(REQUEST, IERROR)
    	INTEGER	REQUEST, IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Start(request, ierror)
    	TYPE(MPI_Request), INTENT(INOUT) :: request
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETER
===============

request

:   Communication request (handle).

OUTPUT PARAMETER
================

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

A communication (send or receive) that uses a persistent request is
initiated by the function MPI_Start.

The argument, request, is a handle returned by one of the persistent
communication-request initialization functions (MPI_Send_init,
MPI_Bsend_init, MPI_Ssend_init, MPI_Rsend_init, MPI_Recv_init). The
associated request should be inactive and becomes active once the call
is made.

If the request is for a send with ready mode, then a matching receive
should be posted before the call is made. From the time the call is made
until after the operation completes, the communication buffer should not
be accessed.

The call is local, with semantics similar to the nonblocking
communication operations (see Section 3.7 in the MPI-1 Standard,
\"Nonblocking Communication.\") That is, a call to MPI_Start with a
request created by MPI_Send_init starts a communication in the same
manner as a call to MPI_Isend; a call to MPI_Start with a request
created by MPI_Bsend_init starts a communication in the same manner as a
call to MPI_Ibsend; and so on.

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

MPI_Bsend_init\
MPI_Rsend_init\
MPI_Send_init\
MPI_Sssend_init\
MPI_Recv_init\
MPI_Startall
