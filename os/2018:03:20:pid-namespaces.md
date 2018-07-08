# PID Namespace

## Creation

Just like other namespaces, the PID namespace can be created via the `clone` c function, using the `CLONE_NEWPID` flag. However, in order to create the `/proc/PID` directories that corresponds to a PID namespace, the proc filesystem needs to be mounted from *within* that PID namespace.

```bash
# We can do this from a shell running inside the PID namespace
mount -t proc proc /mount_point
```

or

```c
// We can do this inside the cloned process
mkdir(mount_point, 0555);
mount("proc", mount_point, "proc", 0, NULL);
```

Having a procfs is necessary for various tools such as ps to work correctly inside the child PID namespace.

## Nested PID Namespaces
PID Namespaces can be nested. By running [multi_pidns.c](https://lwn.net/Articles/532745/) then running the following code:

```bash
$ ls -d /proc4/[1-9]*        Topmost PID namespace created by program
/proc4/1  /proc4/2  /proc4/3  /proc4/4  /proc4/5
$ ls -d /proc3/[1-9]*
/proc3/1  /proc3/2  /proc3/3  /proc3/4
$ ls -d /proc2/[1-9]*
/proc2/1  /proc2/2  /proc2/3
$ ls -d /proc1/[1-9]*
/proc1/1  /proc1/2
$ ls -d /proc0/[1-9]*        Bottommost PID namespace
/proc0/1
```

We can see that each pid namespace only contains its descending namespaces.

# Unshare() and Setns()
The `unshare()` syscall is used to create namespaces. When used, it creates a namespace but it **does not put the caller process into the namespace**, rather, the child processes of the caller process is put into the new namespace.

`setns(fd, 0) /*Second argument can be CLONE_NEWPID to force a check that 'fd' refers to a PID namespace.*/`
The `setns` syscall is used to reassociate thread with a namespace. The `fd` argument is a file descriptor that identifies a namespace that is a descendant of the namespace of the caller; the file descriptor is obtained by opening `proc/PID/ns/NAMESPACE` file for one of the processes in the target namespace.
As with `unshare`, `setns` does not move the caller into the namespace; instead, children that are subsequently created by the caller will be placed in the namespace.
