# Name

`MPI_Init_thread` - Initializes the MPI execution environment

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Init_thread(int *argc, char ***argv,
    int required, int *provided)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_INIT_THREAD(REQUIRED, PROVIDED, IERROR)
    INTEGER    REQUIRED, PROVIDED, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Init_thread(required, provided, ierror)
    INTEGER, INTENT(IN) :: required
    INTEGER, INTENT(OUT) :: provided
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `argc` : C only: Pointer to the number of arguments.
* `argv` : C only: Argument vector.
* `required` : Desired level of thread support (integer).

# Output Parameters

* `provided` : Available level of thread support (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

This routine, or `MPI_Init`, must be called before most other MPI routines
are called. There are a small number of exceptions, such as
`MPI_Initialized` and `MPI_Finalized`. MPI can be initialized at most once;
subsequent calls to `MPI_Init` or `MPI_Init_thread` are erroneous.
`MPI_Init_thread`, as compared to `MPI_Init`, has a provision to request a
certain level of thread support in required:
* `MPI_THREAD_SINGLE` : Only one thread will execute.
:   Only one thread will execute.
* `MPI_THREAD_FUNNELED` : If the process is multithreaded, only the thread that called
:   If the process is multithreaded, only the thread that called
    MPI_Init_thread will make MPI calls.
* `MPI_THREAD_SERIALIZED` : If the process is multithreaded, only one thread will make MPI
:   If the process is multithreaded, only one thread will make MPI
    library calls at one time.
* `MPI_THREAD_MULTIPLE` : If the process is multithreaded, multiple threads may call MPI at
:   If the process is multithreaded, multiple threads may call MPI at
    once with no restrictions.
The level of thread support available to the program is set in
provided. In Open MPI, the value is dependent on how the library was
configured and built. Note that there is no guarantee that provided
will be greater than or equal to required.
Also note that calling `MPI_Init_thread` with a `required` value of
`MPI_THREAD_SINGLE` is equivalent to calling `MPI_Init.`
All MPI programs must contain a call to `MPI_Init` or `MPI_Init_thread.`
Open MPI accepts the C `argc` and `argv` arguments to main, but neither
modifies, interprets, nor distributes them:
            / declare variables /
            MPI_Init_thread(&argc, &argv, req, &prov);
            / parse arguments /
            / main program /
            MPI_Finalize();

# Notes

The Fortran version does not have provisions for `argc` and `argv` and
takes only IERROR.
It is the caller's responsibility to check the value of provided, as
it may be less than what was requested in required.
The MPI Standard does not say what a program can do before an
`MPI_Init_thread` or after an `MPI_Finalize`. In the Open MPI
implementation, it should do as little as possible. In particular, avoid
anything that changes the external state of the program, such as opening
files, reading standard input, or writing to standard output.
`MPI_THREAD_MULTIPLE` Support
## `Mpi_Thread_Multiple` Support

`MPI_THREAD_MULTIPLE` support is included if Open MPI was configured with
the --enable-mpi-thread-multiple configure switch. You can check the
output of `ompi_info(1`) to see if Open MPI has `MPI_THREAD_MULTIPLE`
support:
    shell$ ompi_info | grep -i thread
              Thread support: posix (mpi: yes, progress: no)
    shell$
The "mpi: yes" portion of the above output indicates that Open MPI was
compiled with `MPI_THREAD_MULTIPLE` support.
Note that `MPI_THREAD_MULTIPLE` support is only lightly tested. It likely
does not work for thread-intensive applications. Also note that only
the MPI point-to-point communication functions for the BTL's listed
below are considered thread safe. Other support functions (e.g., MPI
attributes) have not been certified as safe when simultaneously used by
multiple threads.
        tcp
        sm
        mx
        elan
        self
Note that Open MPI's thread support is in a fairly early stage; the
above devices are likely to work, but the latency is likely to be
fairly high. Specifically, efforts so far have concentrated on
correctness, not `performance` (yet).

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

[`MPI_Init(3)`](./?file=MPI_Init.md)
[`MPI_Initialized(3)`](./?file=MPI_Initialized.md)
[`MPI_Finalize(3)`](./?file=MPI_Finalize.md)
[`MPI_Finalized(3)`](./?file=MPI_Finalized.md)