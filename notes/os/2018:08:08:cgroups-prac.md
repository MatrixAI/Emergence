The goal of this exercise is to understand how cgroups works, how to create, migrate, delete cgroups as well as understanding the subsystems in cgroups.


There are multiple differences between cgroup v2 and v1. In this exercise we will mainly look at v2 but also look at v1 for subsystems not provided in v2.

unlike v1, cgroup v2 has only one hierarchy. To mount it, we do

```bash
# mount -t cgroup2 none $MOUNT_POINT
# ls $MOUNT_POINT
cgroup.subtree_control
cgroup.controllers      cgroup.threads
cgroup.max.depth        init.scope
cgroup.max.descendants  system.slice
cgroup.procs            user.slice
cgroup.stat
```



There are different terminologies that we should clarify

## Hierarchy

A cgroup hierarchy is represented by the cgroups filesystem. However it is not just a normal filesystem, it acts as an API for users to communicate with the kernel on cgroups related changes (similar to `/proc` and `/sys`, in fact cgroups is usually mounted under `/sys`).

# Cgroup
A cgroup can be created in the cgoup filesystem simply by using `mkdir $CGROUP_NAME`. A given cgroup can have multiple child cgroups forming a tree structure.


`cgroup.procs` is a read-writable interface file that when read, lists the PIDs of all processes belong to the cgroup. A process can be migrated into a cgroup by writing its PID to the target cgroup's `cgroups.procs`. Only one process can be migrated in a single `write(2)` call. If a process is composed of multiple threads, writing the PID of any thread migrates all threads of the process.


