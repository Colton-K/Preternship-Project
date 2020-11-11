---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Testsome
---

NAME
====

**MPI_Testsome** - Tests for completion of one or more previously
initiated communications in a list.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Testsome(int incount, MPI_Request array_of_requests[],
    	int *outcount, int array_of_indices[],
    	MPI_Status array_of_statuses[])

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_TESTSOME(INCOUNT, ARRAY_OF_REQUESTS, OUTCOUNT,
    		ARRAY_OF_INDICES, ARRAY_OF_STATUSES, IERROR)
    	INTEGER	INCOUNT, ARRAY_OF_REQUESTS(*)
    	INTEGER	OUTCOUNT, ARRAY_OF_INDICES(*)
    	INTEGER	ARRAY_OF_STATUSES(MPI_STATUS_SIZE,*), IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Testsome(incount, array_of_requests, outcount, array_of_indices,
    		array_of_statuses, ierror)
    	INTEGER, INTENT(IN) :: incount
    	TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(incount)
    	INTEGER, INTENT(OUT) :: outcount, array_of_indices(*)
    	TYPE(MPI_Status) :: array_of_statuses(*)
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
================

incount

:   Length of array_of_requests (integer).

array_of_requests

:   Array of requests (array of handles).

OUTPUT PARAMETERS
=================

outcount

:   Number of completed requests (integer).

array_of_indices

:   Array of indices of operations that completed (array of integers).

array_of_statuses

:   Array of status objects for operations that completed (array of
    status).

```{=html}
<!-- -->
```

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

Behaves like MPI_Waitsome, except that it returns immediately.

Returns in outcount the number of requests from the list
array_of_requests that have completed. Returns in the first outcount
locations of the array array_of_indices the indices of these operations
(index within the array array_of_requests; the array is indexed from 0
in C and from 1 in Fortran). Returns in the first outcount locations of
the array array_of_status the status for these completed operations. If
a request that completed was allocated by a nonblocking communication
call, then it is deallocated, and the associated handle is set to
MPI_REQUEST_NULL.

If no operation has completed it returns outcount = 0. If there is no
active handle in the list, it returns outcount = MPI_UNDEFINED.

MPI_Testsome is a local operation, which returns immediately, whereas
MPI_Waitsome blocks until a communication completes, if it was passed a
list that contains at least one active handle. Both calls fulfill a
fairness requirement: If a request for a receive repeatedly appears in a
list of requests passed to MPI_Waitsome or MPI_Testsome, and a matching
send has been posted, then the receive will eventually succeed unless
the send is satisfied by another receive; send requests also fulfill
this fairness requirement.

Errors that occur during the execution of MPI_Testsome are handled as
for MPI_Waitsome.

If your application does not need to examine the *array_of_statuses*
field, you can save resources by using the predefined constant
MPI_STATUSES_IGNORE can be used as a special value for the
*array_of_statuses* argument.

NOTES
=====

The use of MPI_Testsome is likely to be more efficient than the use of
MPI_Testany. The former returns information on all completed
communications; with the latter, a new call is required for each
communication that completes.

A server with multiple clients can use MPI_Waitsome so as not to starve
any client. Clients send messages to the server with service requests.
The server calls MPI_Waitsome with one receive request for each client,
then handles all receives that have completed. If a call to MPI_Waitany
is used instead, then one client could starve while requests from
another client always sneak in first.

ERRORS
======

For each invocation of MPI_Testsome, if one or more requests generate an
MPI error, only the *first* MPI request that caused an error will be
passed to its corresponding error handler. No other error handlers will
be invoked (even if multiple requests generated errors). However, *all*
requests that generate an error will have a relevant error code set in
the corresponding status.MPI_ERROR field (unless MPI_STATUSES_IGNORE was
used).

The default error handler aborts the MPI job, except for I/O function
errors. The error handler may be changed with MPI_Comm_set_errhandler,
MPI_File_set_errhandler, or MPI_Win_set_errhandler (depending on the
type of MPI handle that generated the MPI request); the predefined error
handler MPI_ERRORS_RETURN may be used to cause error values to be
returned. Note that MPI does not guarantee that an MPI program can
continue past an error.

If the invoked error handler allows MPI_Testsome to return to the
caller, the value MPI_ERR_IN_STATUS will be returned in the C and
Fortran bindings.

SEE ALSO
========

MPI_Comm_set_errhandler\
MPI_File_set_errhandler\
MPI_Test\
MPI_Testall\
MPI_Testany\
MPI_Wait\
MPI_Waitall\
MPI_Waitany\
MPI_Waitsome\
MPI_Win_set_errhandler\