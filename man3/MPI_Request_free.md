---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Request_free
---

NAME
====

**MPI_Request_free** - Frees a communication request object.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Request_free(MPI_Request *request)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_REQUEST_FREE(REQUEST, IERROR)
    	INTEGER	REQUEST, IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Request_free(request, ierror)
    	TYPE(MPI_Request), INTENT(INOUT) :: request
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT/OUTPUT PARAMETER
======================

request

:    Communication request (handle).

DESCRIPTION
===========

This operation allows a request object to be deallocated without waiting
for the associated communication to complete.

MPI_Request_free marks the request object for deallocation and sets
request to MPI_REQUEST_NULL. Any ongoing communication that is
associated with the request will be allowed to complete. The request
will be deallocated only after its completion.

NOTES
=====

Once a request is freed by a call to MPI_Request_free, it is not
possible to check for the successful completion of the associated
communication with calls to MPI_Wait or MPI_Test. Also, if an error
occurs subsequently during the communication, an error code cannot be
returned to the user \-- such an error must be treated as fatal.
Questions arise as to how one knows when the operations have completed
when using MPI_Request_free. Depending on the program logic, there may
be other ways in which the program knows that certain operations have
completed and this makes usage of MPI_Request_free practical. For
example, an active send request could be freed when the logic of the
program is such that the receiver sends a reply to the message sent \--
the arrival of the reply informs the sender that the send has completed
and the send buffer can be reused. An active receive request should
never be freed, as the receiver will have no way to verify that the
receive has completed and the receive buffer can be reused.

**Example:**

        CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank)
        IF(rank.EQ.0) THEN
            DO i=1, n
              CALL MPI_ISEND(outval, 1, MPI_REAL, 1, 0, req, ierr)
              CALL MPI_REQUEST_FREE(req, ierr)
              CALL MPI_IRECV(inval, 1, MPI_REAL, 1, 0, req, ierr)
              CALL MPI_WAIT(req, status, ierr)
            END DO
        ELSE    ! rank.EQ.1
            CALL MPI_IRECV(inval, 1, MPI_REAL, 0, 0, req, ierr)
            CALL MPI_WAIT(req, status)
            DO I=1, n-1
               CALL MPI_ISEND(outval, 1, MPI_REAL, 0, 0, req, ierr)
               CALL MPI_REQUEST_FREE(req, ierr)
               CALL MPI_IRECV(inval, 1, MPI_REAL, 0, 0, req, ierr)
               CALL MPI_WAIT(req, status, ierr)
            END DO
            CALL MPI_ISEND(outval, 1, MPI_REAL, 0, 0, req, ierr)
            CALL MPI_WAIT(req, status)
        END IF

This routine is normally used to free persistent requests created with
either *MPI_Recv_init* or *MPI_Send_init* and friends. However, it can
be used to free a request created with *MPI_Irecv* or *MPI_Isend* and
friends; in that case the use can not use the test/wait routines on the
request.

It **is** permitted to free an active request. However, once freed, you
can not use the request in a wait or test routine (e.g., *MPI_Wait* ).

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

MPI_Isend\
MPI_Irecv\
MPI_Issend\
MPI_Ibsend\
MPI_Irsend\
MPI_Recv_init\
MPI_Send_init\
MPI_Ssend_init\
MPI_Rsend_init\
MPI_Test\
MPI_Wait\
MPI_Waitall\
MPI_Waitany\
MPI_Waitsome\
MPI_Testall\
MPI_Testany\
MPI_Testsome
