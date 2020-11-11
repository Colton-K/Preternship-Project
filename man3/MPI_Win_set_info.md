# Name

`MPI_Win_set_info` - Set window info hints

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Win_set_info(MPI_Win win, MPI_Info info)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_WIN_SET_INFO(WIN, INFO, IERROR)
    INTEGER    WIN, INFO, IERROR 
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Win_set_info(win, info, ierror)
    TYPE(MPI_Win), INTENT(IN) :: win
    TYPE(MPI_Info), INTENT(IN) :: info
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `win` : Window on which to set info hints
* `info` : Info object containing hints to be set on *win*

# Output Parameters

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_WIN_SET_INFO` sets new values for the hints of the window associated
with `win`. `MPI_WIN_SET_INFO` is a collective routine. The `info` object
may be different on each process, but any `info` entries that an
implementation requires to be the same on all processes must appear with
the same value in each process's `info` object.

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

[`MPI_Info_create(3)`](./?file=MPI_Info_create.md)
[`MPI_Info_set(3)`](./?file=MPI_Info_set.md)
[`MPI_Info_free(3)`](./?file=MPI_Info_free.md)
[`MPI_Win_get_info(3)`](./?file=MPI_Win_get_info.md)
