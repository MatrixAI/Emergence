# Notes: Using Libcontainer

## Unix/Linux Tools
- `sysctl`: configure kernel paramaters at runtime.
- `chroot`: Run commands in an interactive shell with a special root directory.
- `mount`: Mouting a fs.
- `capabilities`: Specify the capabilities when executing the process inside a container.
- `stat` displays stat of a file
- `getrlimit`, `setrlimit` Set the soft/hard limits of a resource
- `prctl` operations on a process
- `seccomp` Execute some instructions at a restricted environment. In docker's implementation, this is used to control syscalls made in a container, and apply rules/alternative methods of executing a syscall in a non-native environment.


## C Modules
- `clone` similar to fork but with the ability to share resources with parent process. Have flags that allow namespacing.

## Files

## Types
### Config
https://gowalker.org/github.com/opencontainers/runc/libcontainer/configs#Config

## Shared subtrees
1. **Shared mount** Create mirrors of the mount so that mounts and unmounts within any mirrors propagate to the other.
  - `mount --make-shared <mount point>`
1. **Slave mount** Slave mount receives propagation from its master but not vice versa.
1. **Private mount** No propagation ability.
1. **Unbindable mount** a private mount that cannot be cloned through a bind operation.

### Source
[Linux Kernel Archive: Shared Subtrees](https://www.kernel.org/doc/Documentation/filesystems/sharedsubtree.txt)

## Control Group
- `cgroup`: associates a set of tasks with a set of parameters for one or more subsystems.
- `subsystem` is a module that treat groups of tasks in particular ways. Typically a *resource controller*. (e.g. cpusets)
- `hierarchy` is a set of cgroups arranged in a tree, such taht every task in the system is in exactly one of the cgroups, and a set of subsystems.

### Source
[Kernel Doc: Cgroup](https://www.kernel.org/doc/Documentation/cgroup-v1/cgroups.txt)
