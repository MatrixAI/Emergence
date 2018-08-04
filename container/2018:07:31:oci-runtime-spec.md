# OCI Runtime Spec in relation to Matrix's Artifact Spec

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

- [ ] `root` (object, OPTIONAL) specifies container's root fs.
  - [ ] `path` (string, REQUIRED) specifies path to the root fs for the container.
    - `path` can be absolute or relative to the bundle path.
    - The value SHOULD be the conventional `rootfs`
  - [ ] `readonly` (bool, OPTIONAL) if true then the root fs MUST be read-only inside the container, defaults to false.
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
- [ ] `process` (object, OPTIONAL) specifies the container process. This property is REQUIRED when `start` is called.
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




## Linux Specific Configuration
Linux machines have specific configuration parameters available to them, which uses various kernel features like namespaces, cgroups, capabilities, LSM and filesystem jail to fulfill the spec. These specifications can be found [here](https://github.com/opencontainers/runtime-spec/blob/master/config-linux.md).

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
