---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Testany
---

NAME
====

**MPI_Testany** - Tests for completion of any one previously initiated
communication in a list.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Testany(int count, MPI_Request array_of_requests[],
    	int *index, int *flag, MPI_Status *status)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_TESTANY(COUNT, ARRAY_OF_REQUESTS, INDEX, FLAG, STATUS, IERROR)
    	LOGICAL	FLAG
    	INTEGER	COUNT, ARRAY_OF_REQUESTS(*), INDEX
    	INTEGER	STATUS(MPI_STATUS_SIZE), IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Testany(count, array_of_requests, index, flag, status, ierror)
    	INTEGER, INTENT(IN) :: count
    	TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
    	INTEGER, INTENT(OUT) :: index
    	LOGICAL, INTENT(OUT) :: flag
    	TYPE(MPI_Status) :: status
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
================

count

:   List length (integer).

array_of_requests

:   Array of requests (array of handles).

OUTPUT PARAMETERS
=================

index

:   Index of operation that completed, or MPI_UNDEFINED if none
    completed (integer).

flag

:   True if one of the operations is complete (logical).

status

:   Status object (status).

```{=html}
<!-- -->
```

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

MPI_Testany tests for completion of either one or none of the operations
associated with active handles. In the former case, it returns *flag* =
true, returns in *index* the index of this request in the array, and
returns in *status* the status of that operation; if the request was
allocated by a nonblocking communication call then the request is
deallocated and the handle is set to MPI_REQUEST_NULL. (The array is
indexed from 0 in C, and from 1 in Fortran.) In the latter case (no
operation completed), it returns *flag* = false, returns a value of
MPI_UNDEFINED in *index*, and *status* is undefined.

The array may contain null or inactive handles. If the array contains no
active handles then the call returns immediately with *flag* = true,
*index* = MPI_UNDEFINED, and an empty *status*.

If the array of requests contains active handles then the execution of
MPI_Testany(count, array_of_requests, index, status) has the same effect
as the execution of MPI_Test(&*array_of_requests\[i*\], *flag*,
*status*), for *i*=0,1,\...,count-1, in some arbitrary order, until one
call returns *flag* = true, or all fail. In the former case, *index* is
set to the last value of *i*, and in the latter case, it is set to
MPI_UNDEFINED. MPI_Testany with an array containing one active entry is
equivalent to MPI_Test.

If your application does not need to examine the *status* field, you can
save resources by using the predefined constant MPI_STATUS_IGNORE as a
special value for the *status* argument.

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
MPI_TESTANY do not set the status.MPI_ERROR field in the returned
status. The error code is passed to the back-end error handler and may
be passed back to the caller through the return value of MPI_TESTANY if
the back-end error handler returns it. The pre-defined MPI error handler
MPI_ERRORS_RETURN exhibits this behavior, for example.

SEE ALSO
========

MPI_Comm_set_errhandler\
MPI_File_set_errhandler\
MPI_Test\
MPI_Testall\
MPI_Testsome\
MPI_Wait\
MPI_Waitall\
MPI_Waitany\
MPI_Waitsome\
MPI_Win_set_errhandler\
