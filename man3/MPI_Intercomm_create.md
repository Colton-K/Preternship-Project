---
date: "\\#OMPI_DATE\\#"
section: 3
title: MPI_Intercomm_create
---

NAME
====

**MPI_Intercomm_create** - Creates an intercommunicator from two
intracommunicators.

SYNTAX
======

C Syntax
========

    #include <mpi.h>
    int MPI_Intercomm_create(MPI_Comm local_comm, int local_leader,
    	MPI_Comm peer_comm, int remote_leader, int tag, MPI_Comm *newintercomm)

Fortran Syntax
==============

    USE MPI
    ! or the older form: INCLUDE 'mpif.h'
    MPI_INTERCOMM_CREATE(LOCAL_COMM, LOCAL_LEADER, PEER_COMM,
    		REMOTE_LEADER, TAG, NEWINTERCOMM, IERROR)
    	INTEGER	LOCAL_COMM, LOCAL_LEADER, PEER_COMM, REMOTE_LEADER
    	INTEGER	TAG, NEWINTERCOMM, IERROR

Fortran 2008 Syntax
===================

    USE mpi_f08
    MPI_Intercomm_create(local_comm, local_leader, peer_comm, remote_leader,
    		tag, newintercomm, ierror)
    	TYPE(MPI_Comm), INTENT(IN) :: local_comm, peer_comm
    	INTEGER, INTENT(IN) :: local_leader, remote_leader, tag
    	TYPE(MPI_Comm), INTENT(OUT) :: newintercomm
    	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
================

local_comm

:   The communicator containing the process that initiates the
    inter-communication (handle).

local_leader

:   Rank of local group leader in local_comm (integer).

peer_comm

:   \"Peer\" communicator; significant only at the local_leader
    (handle).

remote_leader

:   Rank of remote group leader in peer_comm; significant only at the
    local_leader (integer).

tag

:   Message tag used to identify new intercommunicator (integer).

OUTPUT PARAMETERS
=================

newintercomm

:   Created intercommunicator (handle).

```{=html}
<!-- -->
```

IERROR

:   Fortran only: Error status (integer).

DESCRIPTION
===========

This call creates an intercommunicator. It is collective over the union
of the local and remote groups. Processes should provide identical
local_comm and local_leader arguments within each group. Wildcards are
not permitted for remote_leader, local_leader, and tag.

This call uses point-to-point communication with communicator peer_comm,
and with tag tag between the leaders. Thus, care must be taken that
there be no pending communication on peer_comm that could interfere with
this communication.

If multiple MPI_Intercomm_creates are being made, they should use
different tags (more precisely, they should ensure that the local and
remote leaders are using different tags for each MPI_intercomm_create).

NOTES
=====

We recommend using a dedicated peer communicator, such as a duplicate of
MPI_COMM_WORLD, to avoid trouble with peer communicators.

The MPI 1.1 Standard contains two mutually exclusive comments on the
input intracommunicators. One says that their respective groups must be
disjoint; the other that the leaders can be the same process. After some
discussion by the MPI Forum, it has been decided that the groups must be
disjoint. Note that the **reason** given for this in the standard is
**not** the reason for this choice; rather, the **other** operations on
intercommunicators (like *MPI_Intercomm_merge* ) do not make sense if
the groups are not disjoint.

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

MPI_Intercomm_merge\
MPI_Comm_free\
MPI_Comm_remote_group\
MPI_Comm_remote_size
