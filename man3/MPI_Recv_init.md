---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Recv_init
---

NAME
====

**MPI_Recv_init** - Builds a handle for a receive.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Recv_init(void *buf, int count, MPI_Datatype datatype,
    	int source, int tag, MPI_Comm comm, MPI_Request *request)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_RECV_INIT(BUF, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST,
    		IERROR)
    	<type>	BUF(*)
    	INTEGER	COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Recv_init(buf, count, datatype, source, tag, comm, request, ierror)
    	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
    	INTEGER, INTENT(IN) :: count, source, tag
    	TYPE(MPI_Datatype), INTENT(IN) :: datatype
    	TYPE(MPI_Comm), INTENT(IN) :: comm
    	TYPE(MPI_Request), INTENT(OUT) :: request
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
================

count

:   Maximum number of elements to receive (integer).

datatype

:   Type of each entry (handle).

source

:   Rank of source (integer).

tag

:   Message tag (integer).

comm

:   Communicator (handle).

INPUT/OUTPUT PARAMETER
======================

buf

:   Initial address of receive buffer (choice).

OUTPUT PARAMETERS
=================

request

:   Communication request (handle).

```{=html}
<!-- -->
```

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

Creates a persistent communication request for a receive operation. The
argument *buf* is marked as OUT because the user gives permission to
write on the receive buffer by passing the argument to MPI_Recv_init.

A persistent communication request is inactive after it is created \--
no active communication is attached to the request.

A communication (send or receive) that uses a persistent request is
initiated by the function MPI_Start or MPI_Startall.

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
MPI_Start\
MPI_Startall\
MPI_Request_free
