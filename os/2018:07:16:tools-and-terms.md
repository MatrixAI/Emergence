# Subreaper

subreaper is a flag of a system call `prctl()`, which is used to control operations on a process.

When a process becomes orphaned, it will be reparented to the nearest still living ancestor subreaper.
Subsequently, calls to `getppid()` in the orphaned process will now return the PID of the subreaper process, and when the orphan terminates, it is the subreaper process that will receive a `SIGCHILD` signal and will be able to `wait(2)` on the process to discover its termination status

The flags are: 
- `PR_SET_CHILD_SUBREAPER`: If arg2 is a nonzero, set the "child subreaper" attribute of the calling process, unset the attribute otherwise.
= `PR_GET_CHILD_SUBREAPER`: Return the "child subreaper" setting of the caller, in the location pointed to by `(int *) arg2`.

# SELinux

Security-Enhanced Linux (SELinux) is a linux kernel module that provides a mechanism for supporting access control security policies.

# Rlimit
Each resource has an associated soft and hard limit, as defined by the `rlimit` structure:

```c
struct rlimit {
	rlim_t rlim_cur; /* Soft limit */
	rlim_t rlim_max; /* Hard limit */
}
```

Soft limit is the value that the kernel enforces for the corresponding resoruce. The hard limit acts as a ceiling for the soft limit: an unprivileged process may set only its soft limit to a value in the range from 0 up to the hard limit, and lower its hard limit.

A privileged process (In linux: one with `CAP_SYS_RESOURCE` capability in the initial user namespace) may take arbitrary changes to either limit value.

There are a lot of different resources: Address space, files, CPU, data, locks, POSIX message queues, nice value, fds, threads, virtual pages in RAM, priority, rttime, sigpending, and process stack.


