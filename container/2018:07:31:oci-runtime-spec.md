# OCI Runtime Spec

OCI specification for stand container defines:
1. Configuration file formats
2. A set of standard operations
3. An execution environment

The goal is to create a container that is portable, content-agnostic, infrastructure-agnostic, with self-describing dependencies.

## Filesystem Bundle

A filesystem bundle MUST consists of:

- [ ] a `config.json` file
- [ ] a container's root filesystem, as specified in `config.json`'s `root.path` property.
- [ ] The above artifacts MUST be presented in a single directory on the local filesystem. The directory which contains the bundle is not part of the bundle.

## Lifecycle
- [ ] OCI compliant runtime's create command is invoked with a reference to the location of the bundle and a unique identifier.
- [ ] The container's runtime environment MUST be created according to the configuration in config.json. If the runtime is unable to create the environment specified in the config.json, it MUST generate an error. While the resources requested in the config.json MUST be created, the user-specified program (from process) MUST NOT be run at this time. Any updates to config.json after this step MUST NOT affect the container.
- [ ] Runtime's start command is invoked with the unique identifier of the container.
- [ ] The prestart hooks MUST be invoked by the runtime. If any prestart hook fails, the runtime MUST generate an error, stop the container, and continue the lifecycle at step 9.
- [ ] The runtime MUST run the user-specified program, as specified by process.
- [ ] The poststart hooks MUST be invoked by the runtime. If any poststart hook fails, the runtime MUST log a warning, but the remaining hooks and lifecycle continue as if the hook had succeeded.
- [ ] The container process exits. This MAY happen due to erroring out, exiting, crashing or the runtime's kill operation being invoked.
- [ ] Runtime's delete command is invoked with the unique identifier of the container.
- [ ] The container MUST be destroyed by undoing the steps performed during create phase (step 2).
- [ ] The poststop hooks MUST be invoked by the runtime. If any poststop hook fails, the runtime MUST log a warning, but the remaining hooks and lifecycle continue as if the hook had succeeded.

## Errors and Warnings
- [ ] An error MUST leave the state of the environment as if the operation were never attempted.
- [ ] A warning MUST allow the program to continue as if the warning had not been logged.

## Operations
Runtimes must support the following operations:

### Query state
`state <container-id>`
- [ ] This operation MUST generate an error if it is not provided the ID of a container.
- [ ] Attempting to query a container that does not exist generate an error.
- [ ] This operation MUST return the state of a container as specified [here](https://github.com/opencontainers/runtime-spec/blob/master/runtime.md#state)

### Create
`create <container-id> <path-to-bundle>`
- [ ] This operation MUST generate an error if it is not provided a path to the bundle and the container ID to associate with the container.
- [ ] If the ID provided is not unique across all containers within the scope of the runtime, or is not valid in any other way, the implementation MUST generate an error and a new container MUST NOT be created.
- [ ] This operation MUST create a new container.
- [ ] All of the properties configured in `config.json` except for `process` MUST be applied.
- [ ] `process.args` MUST NOT be applied until triggered by the start operation. The remaining process properties MAY be applied by this operation.
- [ ] If the runtime cannot apply a property as specified in the configuration, it MUST generate an error and a new container MUST NOT be created.
- [ ] The runtime MAY validate `config.json` against this spec, either generically or with respect to the local system capabilities, before creating the container.
- [ ] Any changes made to the config.json file after this operation will not have an effect on the container.

### Start
`start <container-id>`
- [ ] This operation MUST generate an error if it is not provided the container ID.
- [ ] Attempting to start a container that is not created MUST have no effect on the container and MUST generate an error.
- [ ] This operation MUST run the user-specified program as specified by process.
- [ ] This operation MUST generate an error if process was not set.

### Kill
`kill <container-id> <signal>`
- [ ] This operation MUST generate an error if it is not provided the container ID.
- [ ] Attempting to send a signal to a container that is neither created nor running MUST have no effect on the container and MUST generate an error.
- [ ] This operation MUST send the specified signal to the container process.

### Delete
`delete <container-id>`
- [ ] This operation MUST generate an error if it is not provided the container ID.
- [ ] Attempting to delete a container that is not stopped MUST have no effect on the container and MUST generate an error.
- [ ] Deleting a container MUST delete the resources that were created during the create step.
- [ ] Resources associated with the container, but not created by this container, MUST NOT be deleted. Once a container is deleted its ID MAY be used by a subsequent container.

## Configuration
The configuration file contains metadata necessary to implement standard operations against the container. Detailed description (for all OSes) of each field can be found [here](https://github.com/opencontainers/runtime-spec/blob/master/config.md).

### Root
- [ ] `root` (object, OPTIONAL) specifies container's root fs.
  - [ ] `path` (string, REQUIRED) specifies path to the root fs for the container.
    - `path` can be absolute or relative to the bundle path.
    - The value SHOULD be the conventional `rootfs`
  - [ ] `readonly` (bool, OPTIONAL) if true then the root fs MUST be read-only inside the container, defaults to false.

### Mounts
- [ ] `mounts` (array of objects, OPTIONAL) specifies additional mounts beyond `root`.
  - [ ] The runtime MUST mount entries in listed order.
  - [ ] For linux, the parameters are as documented in `mount(2)`.
  - [ ] `destination` (string, REQUIRED) path inside container. MUST be absolute path.
  - [ ] `source` (string, OPTIONAL) a device name, can be a directory name or a dummy. Path values are either absolute or relative to the bundle.
  - [ ] `options` (array of strings, OPTIONAL) mount options of the filesystem to be used.
    - supported optionsa are listed in `mount(8)`.
  - For POSIX platforms the `mount` structure has the following fields
    - [ ] `type` (string, OPTIONAL) the type of the fs to be mounted.
      - [ ] Linux: fs types supported by kernel as listed in `/proc/filesystems`
### Process
`process` (object, OPTIONAL) specifies the container process. This property is REQUIRED when `start` is called.
  - [ ] `terminal` (bool, OPTIONAL) specifies whether a terminal is attached to the process, defaults to false.
  - [ ] `consoleSize` (object, OPTIONAL) specifies the console size in characters of the terminal.
    - This value MUST be ignored if `terminal` is set to `false`
    - [ ] `height` (uint, REQUIRED)
    - [ ] `width` (uint, REQUIRED)
  - [ ] `cwd` (string, REQUIRED) the working directory set for the executable. MUST be absolute path.
  - [ ] `env` (array of strings, OPTIONAL) with the same semantics as [IEEE Std 1003.1-2008 `environ`](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html#tag_08_01)
  - [ ] `args` (array of strings, REQUIRED) with similar semantics to [IEEE Std 1003.1-2008 `execvp`'s argv'](http://pubs.opengroup.org/onlinepubs/9699919799/functions/exec.html)
    - The required entry is used with the same semantics as `execvp`'s *file*.
  - POSIX process:
    - [ ] `rlimits` (array of objects, OPTIONAL) allows setting resource limits for the process.
      - [ ]`type` (string, REQUIRED)
        - Linux: valid values are defined in `getrlimit(2)`.
        -  [ ] The runtime MUST generate an error for any values which cannot be mapped to a relevant kernel interface.
      For the following properties, `rlim` refers to the status returned by the `getrlimit(3)` call.
      - [ ] `soft` (uint64, REQUIRED) the value of the limit enforced for the corresponding resource. `rlim.rlim_cur` MUST match this value.
      - [ ] `hard` (uint64, REQUIRED) the ceiling for the soft limit that could be set by an unprivileged process. `rlim.rlim_max` MUST match this value.
        - Only a privileged process (one with `CAP_SYS_RESOURCE`) can raise a hard limit.
    - `user` (object)
      - `uid` (int, REQUIRED) specifies the user ID in the container namespace.
      - `gid` (int, REQUIRED) specifies the group ID in the container namespace.
      - `additionalGids` (array of ints, OPTIONAL) addtional group gids in the container namespace.
  - Linux processes:
    - [ ] `apparmorProfile` (string, OPTIONAL) name of the AppArmor profile for the process.
    - [ ] `capabilities` (object, OPTIONAL) as defined in `capabilities(7)`.
      - [ ] Any value which cannot be mapped to a relevant kernel interface MUST cause an error.
      - [ ] `effective` (array of strings, OPTIONAL)
      - [ ] `bounding` (array of strings, OPTIONAL)
      - [ ] `inheritable` (array of strings, OPTIONAL)
      - [ ] `permitted` (array of strings, OPTIONAL)
      - [ ] `ambient` (array of strings, OPTIONAL)
    - [ ] `noNewPrivileges` (bool, OPTIONAL)
    - [ ] `oomScoreAdj` (int, OPTIONAL)
      - [ ] If `oomScoreAdj` is set, the runtime MUST set `oom_score_adj` to the given value.
      - [ ] Otherwise , runtime MUST NOT change the value of `oom_score_adj`.
    - [ ] `selinuxLabel` (string, OPTIONAL) specifies the SELinux Label for the process.

### Hostname
- [ ] `hostname` (string, OPTIONAL) specifies the container's hostname as seen by processes running inside the container.

### Linux Specific Configuration
[Source](https://github.com/opencontainers/runtime-spec/blob/master/config-linux.md#memory)

The following are under the `linux` (object, OPTIONAL) property.

- [ ] The following filesystems SHOULD be made available in each container's filesystem:
  - [ ] `/proc` (type proc)
  - [ ] `/sys` (type sysfs)
  - [ ] `/dev/pts` (type devpts)
  - [ ] `/dev/shm` (type tmpfs)
- [ ] `namespaces` (array of objects)
  - [ ] `type` (string, REQUIRED)
    - can be either of `pid`, `network`, `mount`, `ipc`, `uts`, `user`, `cgroup`.
  - [ ] `path` (string, OPTIONAL) namespace file
    - [ ] MUST be absolute path in the runtime mount namespace.
    - [ ] The runtime MUST place the container process in the namespace associated with that `path`.
    - [ ] The runtime MUST generate an error if `path` is not associated with a namespace of type `type`.
    - [ ] If `path` is not specified, runtime MUST create a new container namespace of type `type`.
  - [ ] If a namespace type is not specified, the container MUST inherit the runtme namespace of that type.
  - [ ] Duplicated namespace MUST generate an error.
- [ ] `uidMapping` (array of objects, OPTIONAL) describes the user namespace uid mapping from host to the container.
  - [ ] `containerID` (uint32, REQUIRED) the starting uid of the container
  - [ ] `hostID` (uint32, REQUIRED) starting uid on the host to be mapped
  - [ ] `size` (uint32, REQUIRED) number of ids to be mapped.
- `gidMapping` (array of objects, OPTIONAL) describes the user namespace gid mappings from host to the container.
  - [ ] `containerID` (uint32, REQUIRED) the starting gid of the container
  - [ ] `hostID` (uint32, REQUIRED) starting gid on the host to be mapped
  - [ ] `size` (uint32, REQUIRED) number of ids to be mapped.
- [ ] The runtime SHOULD NOT modify the ownership of referenced filesystems to realise the mapping. Note the number of mapping entries MAY be limited by the kernel.
- [ ] `devices` (array of objects, OPTIONAL) lists devices that MUST be available in the container. THe runtime MAY supply them however it likes.
  - [ ] `type` (string, REQUIRED) - `c`, `b`, `u` or `p` (see [`mknod(1)`](http://man7.org/linux/man-pages/man1/mknod.1.html))
  - [ ] `path` (string, REQUIRED) full path to device inside container.
    - [ ] If a file already exists at `path` that does not match the requested device, the runtime MUST generate an error.
  - [ ] `major, minor` (int64, REQUIRED unless `type` is `p`) [major/minor number of devices](https://www.kernel.org/doc/Documentation/admin-guide/devices.txt)
  - [ ] `fileMode` (uint32, OPTIONAL) file mode for the device
  - [ ] `uid` (uint32, OPTIONAL) id of device owner in the container ns.
  - [ ] `gid` (uint32, OPTIONAL) id of device group in the container ns.
  - [ ] The same `type`, `major` and `minor` SHOULD NOT be used for multiple devices.
  - [ ] In addition to device settings, runtime MUST also supply:
    - `/dev/null`, `/dev/zero`, `/dev/full`, `/dev/random`, `/dev/urandom`, `/dev/tty`, `/dev/console` if `terminal` is enabled in the config by bind mounting the pseudoterminal slave to `/dev/console`. `/dev/ptmx` (A bind-mount or symlink of the container's `/dev/pts/ptmx`)
- [ ] `cgroupPath` (string, OPTIONAL) path to the cgroups.
  - [ ] If absolute, the runtime MUST take the path to be relative to the cgroups mount point.
  - [ ] If relative, the runtime MAY interpret the path relative to a runtime-determined location in the cgroups hierarchy.
  - [ ] If the value is specified, the runtime MUST consistently attach to the same place in the cgroups hierarchy given the same value of `cgroupPath`
  - [ ] If the value is not specified, runtime MAY define default cgroup path.
  - [ ] Runtimes MAY conider certain `cgroupPath` values to be invalid, and MUST generate an error if this is the case.
- [ ] `resources` configures cgroup. Do not specify `resources` unless limits have to be updated.
  - [ ] Runtimes MAY attach the container process to additional cgroup controllers beyond those necessary to fullfill the `resources` settings.
  - [ ] `devices` (array of objects, OPTIONAL) configures the device whitelist
    - [ ] runtime MUST apply entries in the listed order.
    - [ ] `alllow` (boolean, REQUIRED)
    - [ ] `type` (string, OPTIONAL)
    - [ ] `major, minor` (int64, OPTIONAL)
    - [ ] `access` (string, OPTIONAL)
  - [ ] `memory` (object, OPTIONAL) cgroup subsystem `memory`
    - [ ] `limit` (int64, OPTIONAL)
    - [ ] `reservation` (int64, OPTIONAL)
    - [ ] `swap` (int64, OPTIONAL)
    - [ ] `kernel` (int64, OPTIONAL)
    - [ ] `kernelTCP` (int64, OPTIONAL)
    - [ ] `swappiness` (uint64, OPTIONAL)
    - [ ] `disableOOMKiller` (bool, OPTIONAL)
  - [ ] `cpu` (object, OPTIONAL) configure `cpu` and `cpusets` subsystems.
    - [ ] `shares` (uint64, OPTIONAL)
    - [ ] `quota` (int64, OPTIOANL)
    - [ ] `period` (uint64, OPTIONAL)
    - [ ] `realtimeRuntime` (int64, OPTIONAL)
    - [ ] `realtimePeriod` (uint64, OPTIONAL)
    - [ ] `cpus` (string, OPTIONAL)
    - [ ] `mems` (string, OPTIONAL)
  - `blockIO` (object OPTIONAL) confiugre `blkio` subsystem
    - [ ] `weight` (uint16, OPTIONAL)
    - [ ] `leafWeight` (uint16, OPTIONAL)
    - [ ] `weightDevice` (array of objects, OPTIONAL)
      - [ ] `major, minor` (int64, REQUIRED)
      - [ ] `weight` (uint16, OPTIONAL)
      - [ ] `leafWeight` (uint16, OPTIONAL)
      - [ ] at least one of `weight` or `leafWeight` MUST be given.
    - [ ] `throttleReadBpsDevice`, `throttleWriteBpsDevice` (array of objects, OPTIONAL)
      - [ ] `major, minor` (int64, REQUIRED)
      - [ ] `rate` (uint64, REQUIRED)
    - [ ] `throttleReadIOPSDevice`, `throttleWriteIOPSDevice` (array of objects, OPTIONAL)
      - [ ] `major, minor` (uint64, REQUIRED)
      - [ ] `rate` (uint64, REQUIRED)
  - [ ] `hugepagelimits` (array of objects, OPTIONAL)
    - [ ] `pageSize` (string, REQUIRED)
    - [ ] `limit` (uint64, REQUIRED)
  - [ ] `network` (object, OPTIONAL) represent `net_cls` and `net_prio` subsystems
    - [ ] `name` (string, REQUIRED)
    - [ ] `priority` (uint32, REQUIRED)
  - [ ] `pids` (object, OPTIONAL) represents `pids` subsystem
    - [ ] `limit` (int64, REQUIRED)
  - [ ] `rdma` (object, OPTIONAL)
    - [ ] `hcaHandles` (uint32, OPTIONAL)
    - [ ] `hcaObjects` (uint32, OPTIONAL)
    - [ ] At least one of the `hcaHandles` or `hcaObjects` MUST be selected
- [ ] `intelRdt` (object, OPTIONAL) represents the [Intel Resource Director Technology](https://www.kernel.org/doc/Documentation/x86/intel_rdt_ui.txt).
  - [ ] if `intelRdt` is set, the runtime MUST write the containe id to the `<container-id>/tasks` file in a mounted `resctrl` pseudo-filesystem, if no `resctrl` filesystem is available, the runtime MUST generate an error.
  - [ ] `l3CacheSchema` (string, OPTIONAL)
  - [ ] if `l3CacheSchema` is set, runtime MUST write the value to the `schemata` file in the <container-id> directory discussed in `intelRdt`
  - [ ] Otherwise, runtime MUST NOT write to `schemata` files in any `resctrl` pseudo-filesystems
- [ ] `sysctl` (object, OPTIONAL) allows kernel parameters to be modified at runtime.
- [ ] `seccomp` (object, OPTIONAL)
  - [ ] `defaultAction` (string, REQUIRED)
  - [ ] `architectures` (array of strings, OPTIONAL)
  - [ ] `syscalls` (array of objects, OPTIONAL)
    - [ ] `names` (array of strings, REQUIRED)
    - [ ] `action` (string, REQUIRED)
    - [ ] `args` (array of objects, OPTIONAL)
      - [ ] `index` (uint, REQUIRED)
      - [ ] `value` (uint64, REQUIRED)
      - [ ] `valueTwo` (uint64, OPTIONAL)
      - [ ] `op` (string, REQUIRED)
- [ ] `rootfsPropagation` (string, OPTIONAL) sets the rootfs's mount propagation, value is either "slave", "private", "shared" or "unbindable".
- [ ] `maskedPaths` (array of strings, OPTIONAL) will mask over the provided paths inside the container so that they cannot be read.
  - [ ] The value MUST be absolute path in the container namespace.
- [ ] `readonlyPaths` (array of strings, OPTIONAL) set the provided paths as readonly inside the container.
  - [ ] The value MUST be absolute path in the container namespace.
- `mountLabel` (string, OOPTIONAL) will set the SELinux context for the mounts in the container.

### POSIX-platform Hooks
- [ ] `hooks` (object, OPTIONAL) MAY contain any of the following properties:
  - [ ] `prestart` (array of objects, OPTIONAL)
    - [ ] Must be called after the `start` operation is called but **before the user-specified program command is executed**
    - [ ] `path` (string, REQUIRED)
      - [ ] MUST be an absolute path,
      - [ ] runtime MUST resolve this value in the [runtime namespace](https://github.com/opencontainers/runtime-spec/blob/master/glossary.md#runtime-namespace).
    - [ ] `args` (array of strings, OPTIONAL) with the same semantics as IEEE Std 1003.1-2008 execv's argv.
    - [ ] `env` (array of strings, OPTIONAL) with the same semantics as IEEE Std 1003.1-2008's environ.
    - [ ] `timeout` (int, OPTIONAL) is the number of seconds before aborting the hook. If set, timeout MUST be greater than zero.
  - [ ] `poststart` (array of objects, OPTIONAL) Entries in the array have the same schema.
    - [ ] MUST be called **after the user-specified program command is excuted** but before the `start` operation returns.
  - [ ] `poststop` (array of objects, OPTIONAL) Entries in the aray have the same schema.
    - [ ] MUST be called **after the container is deleted** but before the `delete` operation returns.

### Annotations
- [ ] `annotations` (object, OPTIONAL) contains arbitrary metadata for the container.
  - [ ] MAY be structured or unstructured.
  - [ ] MUST be a key-value map.
  - [ ] Empty map or absent are both allowed for this property.

### Extensibility
- [ ] Runtimes MUST NOT generate an error if they encounter an unknwon property. Instead they MUST ignore unknown properties.

### Valid Values
- [ ] Runtimes that are reading or processing this configuration file MUST generate an error when invalid or unsupported values are encountered. Unless support for a valid value is explicitly required, runtimes MAY choose which subset of the valid values it will support.

### File Descriptors
- [ ] The runtime MAY pass additional file descriptors to the application to support features such as [socket activation](http://0pointer.de/blog/projects/socket-activated-containers.html). This in Runc is done through environment variables.
- [ ] Some of the file descriptors MAY be redirected to `/dev/null` even though they are open.
- [ ] When creating the container, runtimes MUST create the following symlinks if the source file exists after processing `mounts`

  |Source|Destination|
  | ---- | --------- |
  |`/proc/self/fd`|`/dev/fd`|
  |`/proc/self/fd/0`|`/dev/stdin`|
  |`/proc/self/fd/1`|`/dev/stdout`|
  |`/proc/self/fd/2`|`/dev/stderr`|
- [ ]
