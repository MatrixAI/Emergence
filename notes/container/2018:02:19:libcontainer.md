# Notes: Libcontainer in more details

## Runc
Files in Runc (excluding its children directories) consists of a set of top level tools dealing with command line interactions.
It uses the library `cli` by `urfave` to handle arguments and events.


- main.go
  - Constructs an `app` from `cli.App` and set helper texts.
  - Add in a list of available commands (as defined in `cli.Command`)
  - Set `runtimeDir` to `XDG_RUNTIME_DIR` is process is not root.
  - Construct `logrus` for error logging.
  - Run the app.

- create.go
  - set up specs using the `cli.Context` object given.
  - calls `startContainer` with `CT_ACT_CREATE` in `utils_linux.go`

- utils_linux.go
  - startContainer()
    - Create a `NotifySocket` with the socketPath given in the context, and host using `NOTIFY_SOCKET` env variable.
    - create a container using the `createContainer` function
    - Create a runner object, run the process using `spec.Process` as arguments.

  - createContainer()
    - create libcontainerconfig using spec and context.
    - `loadFactory(context)`
    - `factory.Create(id, config)`

  - loadFactory()
    - use native cgroupFS to manage cgroup
    - use intel RDT "resource control" filesystem to create and manage Intel Xeon platform shared resources (e.g. L3 cache).
    - calls `New()` in `libcontainer/factory_linux.go`

  - run()
    - Create a new process using config.
    - Assign environment variables LISTEN_FD and LISTEN_PID if needed.
    - Create FDs for extra files and preserved FDs
    - Create a new Signal handler
    - Calls SetupIO() which sets up IO and tty related processes.
    - If `action == CT_ACT_CREATE`: calls `container.Start(process)`

- libcontainer/container_linux.go
  - linuxContainer.Start(process)
    - Create a `exec.fifo` file at container's root with 0000 permission, owned by rootuid and rootgid.
    - calls `linuxContainer.start(process, status == stopped)`
  - linuxContainer.start(process, isInit)
    - Calls newParentProcess() and obtain the init process
    - Calls initProcess.start()

  - newParentProcess()
    - Create a new socket pair called `init` (consists of `init-p` and `init-c`)
    - Create a commandTemplate.
    - If **not** an init process, calls return a `newSetnsProcess`. This process
      encodes the data in netlink processable format.
    - Returns a initProcess

  - BootstrapData()
    - Encode namespace, uid/gid mapping data into netlink readable format.

  - initProcess.start()
    - start the process in a non-blocking way
    - Apply cgroup (which triggers in a series of `mkdirAll()` and writing into `cgroup.procs` for respective cgroups)
    - Apply intel RDT
    - Copy bootstap data into parentPipe
    - execute `initProcess.execSetns()`

  - initProcess.execSetns()
    - Wait for the process cmd to complete
    -

- factory_linux.go
  - New()
    - Create all directories needed for the given `root`
    - Create a LinuxFactory object for **managing** the container created.
    - Calls the list of options (functions) using the factory as the argument
    - return the factory.

  - Create()
    - Validate id and config
    - Make all directories needed for `root/container_id`
    - Change `root/container_id` ownership to current uid and gid
    - Make a `LinuxContainer` object.
    - return the container.
