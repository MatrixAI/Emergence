# Note: Libcontainer process flow on a Linux system.

## Create a container
When a `runc create` command is received:
  1. Load container config from json and commandline args
  1. Use the enum `CT_ACT_CREATE` to tell following functions to create a new process.
  1. Create a **notify socket** - notifies service manager about start-up completion and other service status changes.
  1. Create `libcontainerConfig` using spec and context.
  1. Use native cgroupFS to manage cgroup
  1. Use intel RDT resource control filesystem to create and manage Intel Xeon platform
  shared resources.Â
  1. Create all directories needed for the given `root`
  1. Create a `LinuxFactory` object for the container, which consists of:

    ``` go
    Root string
    InitPath string
    InitArgs []string
    CriuPath string
    NewuidmapPath string
    NewgidmapPath string
    Validator validate.Validator // validates container configurations
    NewCgroupsManager func returning cgroups.Manager
    NewIntelRdtManager func returning intelrdt.Manager
    ```

  1. Validate container configuration
  1. Create directories needed for root/container_id
  1. Change root/container_id ownership to current uid and gid
  1. Make a `LinuxContainer` object:

    ``` go
    id string
    root string
    config *configs.Config
    cgroupManager croups.Manager
    intelRdtManager intelrdt.Manager
    initPath string
    initArgs []string
    initProcess parentProcess
    initProcessStartTime uint64
    criuPath string
    newuidmapPath string
    newgidmapPath string
    m sync.Mutex
    criuVersion int
    state containerState
    created time.Time
    ```

  1. Create a `runner` object:

    ``` go
    enableSubreaper bool
    shouldDestory bool
    detach bool
    listenFDs []*os.File
    preserveFDs int
    pidFile string
    consoleSocket string
    container libcontainer.Container
    action CtAct
    notifySocket *notifySocket
    criuOpts *libcontainer.CriuOpts
    ```

  1. Use the runner object created to run the process in the config
  1. Create a `libcontainer.Process` object from the config:

    ``` go
    Args []string
    Env []string
    User string // Set the uid and gid of the executing process
    AdditionalGroups []string
    Cwd string
    Stdin io.Reader
    Stdout io.Writer
    ExtraFiles []*os.File
    ConsoleWidth uint16
    ConsoleHeight uint16
    Capabilities *configs.Capabilities
    AppArmorProfile string
    Label string
    Rlimits []configs.Rlimit
    ConsoleSocket *os.File
    ops processOperations
    ```

  1. Assign enviornment variables LISTEN_FD and LISTEN_PID.
  1. Create a Signal Handler

    ```
    signals chan os.signal
    notifySocket *notifySocket
    ```

  1. If `Terminal=True` in the config file, create a TTY program and connect it with a console sockPair.
  1. Create a `exec.fifo` file at container's root with 0000 permission, owned by rootuid and rootgid.
  1. Create a init socket pair
  1. Create a commandTemplate - `Cmd` object:

    ``` go
    Path string // path of the command to run
    Args []string
    Env []string
    Dir string // working directory of the command
    Stdin io.Reader // If nil, read from /dev/null
    Stdout io.Writer // If nil, output to /dev/null
    Stderr io.Writer // If nil, output to /dev/null
    ExtraFiles []*os.File
    SysProcAttr *syscall.SysProcAttr
    Process *os.Process
    ProcessState *os.ProcessState
    ctx context.Context
    lookPathErr error
    finished bool
    childFiles []*os.File
    closeAfterStart []io.Closer
    closeAfterWait []io.Closer
    goroutine []func() error
    errch chan error //one send per goroutine
    waitDone chan struct{}
    ```

    `os.Process` type:

    ``` go
    Pid int
    handle uintptr // handle is accessed atomically on Windows
    isdone uint32 // process has been successfully waited on, non zero if true.
    sigMu sync.RWMutex // avoid race between wait and signal
    ```

  1. Creates a `InitProcess`:

    ``` go
    cmd *exec.Cmd
    parentPipe *os.File
    childPipe *os.File
    config *initConfig
    manager cgroups.Manager
    intelRdtManager intelrdt.Manager
    container *linuxcontainer
    fds []string
    process *Process
    bootstrapData io.Reader // Contains information with namespaces
    sharePidns bool
    ```

  1. Start the process in a non-blocking way.
    1. set up file descriptors
  1. Apply cgroup setting to the process: Find mount points, `mkdirall` for all cgroup subsystems we
    wish to apply, then write the process's pid into `cgroup.procs`
  1. Apply intelRdt setting to the process
  1. Copy the bootstrap data to the parentPipe
  1. `setns` on the process:
  1. Duplicate file descriptor names before the container process can move them.
  1. set external descriptors
  1. Create network interfaces (Network struct looks like this:

    ``` go
      Type string // commonly veth and loopback
      Name string
      Bridge string
      MacAddress string
      Address string
      Gateway string
      IPv6Address string
      IPv6Gateway string
      Mtu int // maximum transmission unit
      TxQueueLen int // Transmit queue length
    ```

  1. Send config to init process

## Create a container (From `strace`)
1. Load commandline arguments
1. Load config.json
1. fstate all cgroup files.
1. read `/proc/self/cgroup`
1. `mkdirat /sys/fs/cgroup/devices/user.slice/c1` , repeat this for other cgroup categories.
1. create two socketpairs: `PF_LOCAL SOCK_STREAM`
1. `unshare` with various namespace flags
1. For the *unshared* process:
  1. Set uid and gid to 0 (for the UTS namespace)
  1. Set signals handlers
  1. mount various files using `mount`, `mkdirat` and `mknodat` (for special files)
  1. remount container's filesystem (reason unknown)
  1. Set capabilities using `prctl`
1. Exit parent processes, clean up useless file descriptors


## Questions
- How exactly does initProcess.start() work? When does CLONE(NS_FLAGS) apply?
- How does netlink comes into use?
