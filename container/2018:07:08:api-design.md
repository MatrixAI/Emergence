# hs-libcontainer API


The hs-libcontainer API should include everything that Docker can achieve, but at the same time be flexible enough so our orchestrator is able to manipulate the container in many ways.

Docker essentially shells out to runc, that means that runc's interface is enough to be able to achieve everything that Docker needs to deal with containers. Maybe having an interface as such would be ideal, this means that all low level container related tasks are dealt with in Go. I should investigate the top level design first before I dive straight into serialisation.

## Runc's interface
- State
  - Returns a json style output, which needs to be serialised.
- Create
  - Options:
    - bundle, console-socket, pid-file, no-pivot, no-new-keyring, preserve-fds
    - no-new-keyring would cause the container to inherit the calling processes session keyring.
  - What needs to be serialised:
    - Bundle config content, and maybe even the filesystem tarball even.
    - preserve-fds (`$LISTEN_FDS`)
- Start


## OCI Runtime Specification

[OCI Runtime Operations Specification](https://github.com/opencontainers/runtime-spec/blob/master/runtime.md)
contains details of each operation.

Notation: `API-name(<arg1>, <arg2>...): <return-value>`

- Query state
  - Minimal requirement: `state(container-id): (State, error)`
  - Contains:
    - ociVersion
    - id
    - status (creating, created, running, stopped)
    - pid (id of container process seen by host)
    - bundle
    - annotations (key-value, optional)
- Create
  - Minimal requirement: `create(container-id, bundle-path): error`
  - bundle contains `config.json` and the container filesystem.
  - All properties in `config.json` are applied except `process`.
  - `process.args` MUST NOT be applied until triggered by `start`
  - Remaining `process` properties MAY be applied.
  - Changes to `config.json` after this operation will not effect the container.
    - This means that `process.args` though unapplied, has to be stored somewhere in memory (?).
  - Runtimes MUST create symlinks if the source file exists after processing `mounts` like [this](https://github.com/opencontainers/runtime-spec/blob/master/runtime-linux.md#-dev-symbolic-links)
- Start
  - Minimal requirement: `start(container-id): error`
  - This operation MUST run the user-specified program as specified in `process`.
- Kill
  - Minimal requriements: `kill(container-id, Signal): error`
  - MUST send the specified signal to the container process.
- Delete
  - Minimal requirements: `delete(container-id): error`
  - Attempting to delete a container that is not `stopped` MUST generate an error.

### Life Cycle of a Container

 The lifecycle describes the timeline of events that happen from when a container is created to when it ceases to exist.

1. OCI compliant runtime's create command is invoked with a reference to the location of the bundle and a unique identifier.
1. The container's runtime environment MUST be created according to the configuration in config.json. If the runtime is unable to create the environment specified in the config.json, it MUST generate an error. While the resources requested in the config.json MUST be created, the user-specified program (from process) MUST NOT be run at this time. Any updates to config.json after this step MUST NOT affect the container.
1. Runtime's start command is invoked with the unique identifier of the container.
1. The prestart hooks MUST be invoked by the runtime. If any prestart hook fails, the runtime MUST generate an error, stop the container, and continue the lifecycle at step 9.
1. The runtime MUST run the user-specified program, as specified by process.
1. The poststart hooks MUST be invoked by the runtime. If any poststart hook fails, the runtime MUST log a warning, but the remaining hooks and lifecycle continue as if the hook had succeeded.
1. The container process exits. This MAY happen due to erroring out, exiting, crashing or the runtime's kill operation being invoked.
1. Runtime's delete command is invoked with the unique identifier of the container.
1. The container MUST be destroyed by undoing the steps performed during create phase (step 2).
1. The poststop hooks MUST be invoked by the runtime. If any poststop hook fails, the runtime MUST log a warning, but the remaining hooks and lifecycle continue as if the hook had succeeded.

[Source](https://github.com/opencontainers/runtime-spec/blob/master/runtime.md#lifecycle)

## Implementation

Runc actually makes a lot of assumptions on the filesystem, because it is a one-time program (i.e. not a daemon), it saves the container information at `/run/runc` if the container is created by root user, and a custom location otherwise. There are also other dependencies such as socket location, bundle path, config path, rootfs path, not mentioning one of the underlying technology: cgroups relies on a virtual fs as its API. Runc already has the config file parser set up, is it really worth it to go through the hassle of serialising the entire config file for the sake of not interacting with the filesystem?
