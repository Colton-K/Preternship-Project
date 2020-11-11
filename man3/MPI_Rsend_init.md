---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Rsend_init
---

NAME
====

**MPI_Rsend_init** - Builds a handle for a ready send.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Rsend_init(const void *buf, int count, MPI_Datatype datatype,
    	int dest, int tag, MPI_Comm comm, MPI_Request *request)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_RSEND_INIT(BUF, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST,
    		IERROR)
    	<type>	BUF(*)
    	INTEGER	COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Rsend_init(buf, count, datatype, dest, tag, comm, request, ierror)
    	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
    	INTEGER, INTENT(IN) :: count, dest, tag
    	TYPE(MPI_Datatype), INTENT(IN) :: datatype
    	TYPE(MPI_Comm), INTENT(IN) :: comm
    	TYPE(MPI_Request), INTENT(OUT) :: request
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
================

buf

:   Initial address of send buffer (choice).

count

:   Number of elements sent (integer).

datatype

:   Type of each element (handle).

dest

:   Rank of destination (integer).

tag

:   Message tag (integer).

comm

:   Communicator (handle).

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

Creates a persistent communication object for a ready mode send
operation, and binds to it all the arguments of a send operation.

A communication (send or receive) that uses a persistent request is
initiated by the function MPI_Start.

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
MPI_Send_init\
MPI_Sssend_init\
MPI_Recv_init\
MPI_Start\
MPI_Startall\
MPI_Request_free
