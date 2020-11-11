---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Testall
---

NAME
====

**MPI_Testall** - Tests for the completion of all previously initiated
communications in a list.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Testall(int count, MPI_Request array_of_requests[],
    	int *flag, MPI_Status array_of_statuses[])

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_TESTALL(COUNT, ARRAY_OF_REQUESTS, FLAG, ARRAY_OF_STATUSES,
    		IERROR)
    	LOGICAL	FLAG
    	INTEGER	COUNT, ARRAY_OF_REQUESTS(*)
    	INTEGER	ARRAY_OF_STATUSES(MPI_STATUS_SIZE,*), IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Testall(count, array_of_requests, flag, array_of_statuses, ierror)
    	INTEGER, INTENT(IN) :: count
    	TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
    	LOGICAL, INTENT(OUT) :: flag
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

flag

:   True if previously initiated communications are complete (logical.)

array_of_statuses

:   Array of status objects (array of status).

```{=html}
<!-- -->
```

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

Returns *flag* = true if all communications associated with active
handles in the array have completed (this includes the case where no
handle in the list is active). In this case, each status entry that
corresponds to an active handle request is set to the status of the
corresponding communication; if the request was allocated by a
nonblocking communication call then it is deallocated, and the handle is
set to MPI_REQUEST_NULL. Each status entry that corresponds to a null or
inactive handle is set to empty.

Otherwise, *flag* = false is returned, no request is modified and the
values of the status entries are undefined. This is a local operation.

If your application does not need to examine the *array_of_statuses*
field, you can save resources by using the predefined constant
MPI_STATUSES_IGNORE can be used as a special value for the
*array_of_statuses* argument.

Errors that occurred during the execution of MPI_Testall are handled in
the same manner as errors in MPI_Waitall.

NOTE
====

*flag* is true only if all requests have completed. Otherwise, *flag* is
false, and neither *array_of_requests* nor *array_of_statuses* is
modified.

ERRORS
======

For each invocation of MPI_Testall, if one or more requests generate an
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

If the invoked error handler allows MPI_Testall to return to the caller,
the value MPI_ERR_IN_STATUS will be returned in the C and Fortran
bindings.

SEE ALSO
========

MPI_Comm_set_errhandler\
MPI_File_set_errhandler\
MPI_Test\
MPI_Testany\
MPI_Testsome\
MPI_Wait\
MPI_Waitall\
MPI_Waitany\
MPI_Waitsome\
MPI_Win_set_errhandler\
