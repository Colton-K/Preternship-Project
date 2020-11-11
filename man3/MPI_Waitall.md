---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Waitall
---

NAME
====

**MPI_Waitall** - Waits for all given communications to complete.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Waitall(int count, MPI_Request array_of_requests[],
    	MPI_Status *array_of_statuses)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_WAITALL(COUNT, ARRAY_OF_REQUESTS, ARRAY_OF_STATUSES, IERROR)
    	INTEGER	COUNT, ARRAY_OF_REQUESTS(*)
    	INTEGER	ARRAY_OF_STATUSES(MPI_STATUS_SIZE,*), IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Waitall(count, array_of_requests, array_of_statuses, ierror)
    	INTEGER, INTENT(IN) :: count
    	TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
    	TYPE(MPI_Status) :: array_of_statuses(*)
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
================

count

:   Lists length (integer).

array_of_requests

:   Array of requests (array of handles).

OUTPUT PARAMETERS
=================

array_of_statuses

:   Array of status objects (array of status).

```{=html}
<!-- -->
```

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

Blocks until all communication operations associated with active handles
in the list complete, and returns the status of all these operations
(this includes the case where no handle in the list is active). Both
arrays have the same number of valid entries. The ith entry in
array_of_statuses is set to the return status of the ith operation.
Requests that were created by nonblocking communication operations are
deallocated, and the corresponding handles in the array are set to
MPI_REQUEST_NULL. The list may contain null or inactive handles. The
call sets to empty the status of each such entry.

The error-free execution of MPI_Waitall(count, array_of_requests,
array_of_statuses) has the same effect as the execution of
MPI_Wait(&array_of_request\[i\], &array_of_statuses\[i\]), for
i=0,\...,count-1, in some arbitrary order. MPI_Waitall with an array of
length 1 is equivalent to MPI_Wait.

When one or more of the communications completed by a call to
MPI_Waitall fail, it is desirable to return specific information on each
communication. The function MPI_Waitall will return in such case the
error code MPI_ERR_IN_STATUS and will set the error field of each status
to a specific error code. This code will be MPI_SUCCESS if the specific
communication completed; it will be another specific error code if it
failed; or it can be MPI_ERR_PENDING if it has neither failed nor
completed. The function MPI_Waitall will return MPI_SUCCESS if no
request had an error, or will return another error code if it failed for
other reasons (such as invalid arguments). In such cases, it will not
update the error fields of the statuses.

If your application does not need to examine the *array_of_statuses*
field, you can save resources by using the predefined constant
MPI_STATUSES_IGNORE can be used as a special value for the
*array_of_statuses* argument.

ERRORS
======

For each invocation of MPI_Waitall, if one or more requests generate an
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

If the invoked error handler allows MPI_Waitall to return to the caller,
the value MPI_ERR_IN_STATUS will be returned in the C and Fortran
bindings.

SEE ALSO
========

MPI_Comm_set_errhandler\
MPI_File_set_errhandler\
MPI_Test\
MPI_Testall\
MPI_Testany\
MPI_Testsome\
MPI_Wait\
MPI_Waitany\
MPI_Waitsome\
MPI_Win_set_errhandler\
