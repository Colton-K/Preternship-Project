---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Wait
---

NAME
====

**MPI_Wait** - Waits for an MPI send or receive to complete.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Wait(MPI_Request *request, MPI_Status *status)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_WAIT(REQUEST, STATUS, IERROR)
    	INTEGER	REQUEST, STATUS(MPI_STATUS_SIZE), IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Wait(request, status, ierror)
    	TYPE(MPI_Request), INTENT(INOUT) :: request
    	TYPE(MPI_Status) :: status
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETER
===============

request

:   Request (handle).

OUTPUT PARAMETERS
=================

status

:   Status object (status).

```{=html}
<!-- -->
```

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

A call to MPI_Wait returns when the operation identified by request is
complete. If the communication object associated with this request was
created by a nonblocking send or receive call, then the object is
deallocated by the call to MPI_Wait and the request handle is set to
MPI_REQUEST_NULL.

The call returns, in status, information on the completed operation. The
content of the status object for a receive operation can be accessed as
described in Section 3.2.5 of the MPI-1 Standard, \"Return Status.\" The
status object for a send operation may be queried by a call to
MPI_Test_cancelled (see Section 3.8 of the MPI-1 Standard, \"Probe and
Cancel\").

If your application does not need to examine the *status* field, you can
save resources by using the predefined constant MPI_STATUS_IGNORE as a
special value for the *status* argument.

One is allowed to call MPI_Wait with a null or inactive request
argument. In this case the operation returns immediately with empty
status.

NOTES
=====

Successful return of MPI_Wait after an MPI_Ibsend implies that the user
send buffer can be reused i.e., data has been sent out or copied into a
buffer attached with MPI_Buffer_attach. Note that, at this point, we can
no longer cancel the send (for more information, see Section 3.8 of the
MPI-1 Standard, \"Probe and Cancel\"). If a matching receive is never
posted, then the buffer cannot be freed. This runs somewhat counter to
the stated goal of MPI_Cancel (always being able to free program space
that was committed to the communication subsystem).

Example: Simple usage of nonblocking operations and MPI_Wait.

        CALL MPI_COMM_RANK(comm, rank, ierr)
        IF(rank.EQ.0) THEN
            CALL MPI_ISEND(a(1), 10, MPI_REAL, 1, tag, comm, request, ierr)
            **** do some computation ****
            CALL MPI_WAIT(request, status, ierr)
        ELSE
            CALL MPI_IRECV(a(1), 15, MPI_REAL, 0, tag, comm, request, ierr)
            **** do some computation ****
            CALL MPI_WAIT(request, status, ierr)
        END IF

ERRORS
======

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
MPI_Comm_set_errhandler, MPI_File_set_errhandler, or
MPI_Win_set_errhandler (depending on the type of MPI handle that
generated the request); the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

Note that per MPI-1 section 3.2.5, MPI errors on requests passed to
MPI_WAIT do not set the status.MPI_ERROR field in the returned status.
The error code is passed to the back-end error handler and may be passed
back to the caller through the return value of MPI_WAIT if the back-end
error handler returns it. The pre-defined MPI error handler
MPI_ERRORS_RETURN exhibits this behavior, for example.

SEE ALSO
========

MPI_Comm_set_errhandler\
MPI_File_set_errhandler\
MPI_Test\
MPI_Testall\
MPI_Testany\
MPI_Testsome\
MPI_Waitall\
MPI_Waitany\
MPI_Waitsome\
MPI_Win_set_errhandler\
