# Notes: Libcontainer Project Structure

## Modules
### AppArmor
AppArmor is a Linux kernel security module that allows the system administrator
to restrict programs' capabilities with per-program profiles.

### CGroups
Contains a cgroup manager that manages and applies the cgroup
settings for each container.

- `cgroup.go` Contains an interface `CGroupManager`
- `stats.go` Contains structs for the stat generated for each cgroup subsystem.
- `utils.go` Contains utils dealing with cgroup mount point, set `cgroup.proc`, `subsystem`.

#### cgroup/fs
Utilities to manage each cgroup subsystem:
- `Apply_raw.go`: Contains `subsystem` interface
- `blkio.go`: Sets limits on I/O access to and from block devices such as physical drives (disk, SSD or USB)
- `cpu.go`: Uses the scheduler to provide cgroup tasks access to the CPU.
- `cpuacct`: Generates automatic reports on CPU resources used by tasks in a cgroup.
- `cputset`: Assigns individual CPUs (on a multicore system) and memory nodes to tasks in a cgroup.
- `devices`: Allows or denies access to devices by tasks in a cgroup.
- `freezer`: Suspends or resumes tasks in a cgroup.
- `hugetlb`: Allows to limit the HugeTLB usage per cgroup and enforces the controller limit during page fault.
- `memory`: Sets limit on memory use by tasks in a cgroup and generates automatic reports on memory resources used by those tasks.
- `net_cls`: tags network packets with a class identifier (classid) that allows Linux traffic controller (`tc`) to identify packets originating from a particular cgroup task. // TODO: Needs more reading
- `net_prio`: Provides a way to dynamically set the priority of network traffic per network interface.
- `perf_event`: Identifies cgroup membership of tasks and can be used for performance analysis
- `pids`: Allow a cgroup hierarchy to stop any new tasks from being fork()'d or clone()'d after a certain limit is reached.
- `name`: Support operations towards a cgroup.

#### cgroup/systemd
- `Get`: Get a subsystem from a subsystemSet via its name.
