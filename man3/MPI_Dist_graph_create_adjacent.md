# Name

MPI_Dist_graph_create_adjacent  - Makes a new communicator to which
topology information has been attached.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Dist_graph_create_adjacent(MPI_Comm comm_old, int indegree, const int sources[],
    const int sourceweights[], int outdegree, const int destinations[], const int destweights[],

        MPI_Info info, int reorder, MPI_Comm *comm_dist_graph)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_DIST_GRAPH_CREATE_ADJACENT(COMM_OLD, INDEGREE, SOURCES, SOURCEWEIGHTS, OUTDEGREE,
                DESTINATIONS, DESTWEIGHTS, INFO, REORDER, COMM_DIST_GRAPH, IERROR)
    INTEGER    COMM_OLD, INDEGREE, SOURCES(*), SOURCEWEIGHTS(*), OUTDEGREE, DESTINATIONS(*), DESTWEIGHTS(*), INFO
    INTEGER    COMM_DIST_GRAPH, IERROR
    LOGICAL REORDER
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Dist_Graph_create_adjacent(comm_old, ndegree, sources, sourceweights,
        outdegree, destinations, destweights, info, reorder,
        comm_dist_graph, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm_old
    INTEGER, INTENT(IN) :: indegree, sources(indegree), outdegree, destinations(outdegree)
    INTEGER, INTENT(IN) :: sourceweights(*), destweights(*)
    TYPE(MPI_Info), INTENT(IN) :: info
    LOGICAL, INTENT(IN) :: reorder
    TYPE(MPI_Comm), INTENT(OUT) :: comm_dist_graph
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `comm_old` : Input communicator without topology (handle).
* `indegree` : Size of *sources* and *sourceweights* arrays (non-negative integer).
* `sources` : Ranks of processes for which the calling process is a destination
(array of non-negative integers).
* `sourceweights` : Weights of the edges into the calling process (array of non-negative
integers).
* `outdegree` : Size of *destinations* and *destweights* arrays (non-negative
integer).
* `destinations` : Ranks of processes for which the calling process is a source (array
of non-negative integers).
* `destweights` : Weights of the edges out of the calling process (array of
non-negative integers).
* `info` : Hints on optimization and interpretation of weights (handle).
* `reorder` : Ranking may be reordered (true) or not (false) (logical).
```


# Output Parameters

* `comm_dist_graph` : Communicator with distributed graph topology added (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Dist_graph_create_adjacent` creats a new communicator
`comm_dist_graph` with distrubuted graph topology and returns a handle
to the new communicator. The number of processes in `comm_dist_graph` is
identical to the number of processes in `comm_old`. Each process passes
all information about its incoming and outgoing edges in the virtual
distributed graph topology. The calling processes must ensure that each
edge of the graph is described in the source and in the destination
process with the same weights. If there are multiple edges for a given
(source,dest) pair, then the sequence of the weights of these edges does
not matter. The complete communication topology is the combination of
all edges shown in the `sources` arrays of all processes in `comm_old,`
which must be identical to the combination of all edges shown in the
`destinations` arrays. Source and destination ranks must be process
ranks of `comm_old`. This allows a fully distributed specication of the
communication graph. Isolated processes (i.e., processes with no
outgoing or incoming edges, that is, processes that have specied
indegree and `outdegree` as zero and thus do not occur as source or
destination rank in the graph specication) are allowed. The call to
`MPI_Dist_graph_create_adjacent` is collective.

# Weights

Weights are specied as non-negative integers and can be used to
influence the process remapping strategy and other internal MPI
optimizations. For instance, approximate count arguments of later
communication calls along specic edges could be used as their edge
weights. Multiplicity of edges can likewise indicate more intense
communication between pairs of processes. However, the exact meaning of
edge weights is not specied by the MPI standard and is left to the
implementation. An application can supply the special value
`MPI_UNWEIGHTED` for the weight array to indicate that all edges have the
same (effectively no) weight. It is erroneous to supply `MPI_UNWEIGHTED`
for some but not all processes of `comm_old`. If the graph is weighted but
`indegree` or `outdegree` is zero, then `MPI_WEIGHTS_EMPTY` or any
arbitrary array may be passed to `sourceweights` or destweights
respectively. Note that `MPI_UNWEIGHTED` and `MPI_WEIGHTS_EMPTY` are not
special weight values; rather they are special values for the total
array argument. In Fortran, `MPI_UNWEIGHTED` and `MPI_WEIGHTS_EMPTY` are
objects like `MPI_BOTTOM` (not usable for initialization or assignment).
See MPI-3 § 2.5.4.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

# See Also

[`MPI_Dist_graph_neighbors(3)`](./?file=MPI_Dist_graph_neighbors.md)
[`MPI_Dist_graph_create(3)`](./?file=MPI_Dist_graph_create.md)
[`MPI_Dist_graph_neighbors_count(3)`](./?file=MPI_Dist_graph_neighbors_count.md)
