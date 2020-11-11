---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Test
---

NAME
====

**MPI_Test** - Tests for the completion of a specific send or receive.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Test(MPI_Request *request, int *flag, MPI_Status *status)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_TEST(REQUEST, FLAG, STATUS, IERROR)
    	LOGICAL	FLAG
    	INTEGER	REQUEST, STATUS(MPI_STATUS_SIZE), IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Test(request, flag, status, ierror)
    	TYPE(MPI_Request), INTENT(INOUT) :: request
    	LOGICAL, INTENT(OUT) :: flag
    	TYPE(MPI_Status) :: status
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETER
===============

request

:   Communication request (handle).

OUTPUT PARAMETERS
=================

flag

:   True if operation completed (logical).

status

:   Status object (status).

```{=html}
<!-- -->
```

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

A call to MPI_Test returns flag = true if the operation identified by
request is complete. In such a case, the status object is set to contain
information on the completed operation; if the communication object was
created by a nonblocking send or receive, then it is deallocated and the
request handle is set to MPI_REQUEST_NULL. The call returns flag =
false, otherwise. In this case, the value of the status object is
undefined. MPI_Test is a local operation.

The return status object for a receive operation carries information
that can be accessed as described in Section 3.2.5 of the MPI-1
Standard, \"Return Status.\" The status object for a send operation
carries information that can be accessed by a call to MPI_Test_cancelled
(see Section 3.8 of the MPI-1 Standard, \"Probe and Cancel\").

If your application does not need to examine the *status* field, you can
save resources by using the predefined constant MPI_STATUS_IGNORE as a
special value for the *status* argument.

One is allowed to call MPI_Test with a null or inactive *request*
argument. In such a case the operation returns with *flag* = true and
empty *status*.

The functions MPI_Wait and MPI_Test can be used to complete both sends
and receives.

NOTES
=====

The use of the nonblocking MPI_Test call allows the user to schedule
alternative activities within a single thread of execution. An
event-driven thread scheduler can be emulated with periodic calls to
MPI_Test.

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
MPI_TEST do not set the status.MPI_ERROR field in the returned status.
The error code is passed to the back-end error handler and may be passed
back to the caller through the return value of MPI_TEST if the back-end
error handler returns it. The pre-defined MPI error handler
MPI_ERRORS_RETURN exhibits this behavior, for example.

SEE ALSO
========

MPI_Comm_set_errhandler\
MPI_File_set_errhandler\
MPI_Testall\
MPI_Testany\
MPI_Testsome\
MPI_Wait\
MPI_Waitall\
MPI_Waitany\
MPI_Waitsome\
MPI_Win_set_errhandler\
