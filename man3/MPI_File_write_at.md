---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_File_write_at
---

NAME
====

**MPI_File_write_at** - Writes a file at an explicitly specified offset
(blocking, noncollective).

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_File_write_at(MPI_File fh, MPI_Offset offset, const void *buf,
    	int count, MPI_Datatype datatype, MPI_Status *status)

Fortran Syntax (see FORTRAN 77 NOTES)
=====================================

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_FILE_WRITE_AT(FH, OFFSET, BUF, COUNT,
    	DATATYPE, STATUS, IERROR)
    	<type>	BUF(*)
    	INTEGER	FH, COUNT, DATATYPE, STATUS(MPI_STATUS_SIZE), IERROR
    	INTEGER(KIND=MPI_OFFSET_KIND)	OFFSET

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_File_write_at(fh, offset, buf, count, datatype, status, ierror)
    	TYPE(MPI_File), INTENT(IN) :: fh
    	INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
    	TYPE(*), DIMENSION(..), INTENT(IN) :: buf
    	INTEGER, INTENT(IN) :: count
    	TYPE(MPI_Datatype), INTENT(IN) :: datatype
    	TYPE(MPI_Status) :: status
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
================

fh

:   File handle (handle).

offset

:   File offset (integer).

buf

:   Initial address of buffer (choice).

count

:   Number of elements in buffer (integer).

datatype

:   Data type of each buffer element (handle).

OUTPUT PARAMETERS
=================

status

:   Status object (status).

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

MPI_File_write_at attempts to write into the file associated with *fh*
(at the *offset* position) a total number of *count* data items having
*datatype* type from the user\'s buffer *buf.* The offset is in *etype*
units relative to the current view. That is, holes are not counted when
locating an offset. The data is written into those parts of the file
specified by the current view. MPI_File_write_at stores the number of
*datatype* elements actually written in *status.* All other fields of
*status* are undefined.

It is erroneous to call this function if MPI_MODE_SEQUENTIAL mode was
specified when the file was opened.

FORTRAN 77 NOTES
================

The MPI standard prescribes portable Fortran syntax for the *OFFSET*
argument only for Fortran 90. FORTRAN 77 users may use the non-portable
syntax

         INTEGER*MPI_OFFSET_KIND OFFSET

where MPI_OFFSET_KIND is a constant defined in mpif.h and gives the
length of the declared integer in bytes.

ERRORS
======

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
MPI_ERRORS_RETURN. The error handler may be changed with
MPI_File_set_errhandler; the predefined error handler
MPI_ERRORS_ARE_FATAL may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.

SEE ALSO
========

MPI_File_iwrite_at\
MPI_File_write_at_all\
MPI_File_write_at_all_begin\
MPI_File_write_at_all_end\
