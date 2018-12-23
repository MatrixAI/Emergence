{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module OCI.RuntimeSpec where

import GHC.Generics
import Data.Aeson 
import Data.Word
import Data.Int
import qualified Data.Map as Map

stripLabel :: String -> String -> String
stripLabel _ (y:ys)
  | y == '_' = ys
stripLabel (x:xs) (y:ys)
  | x == y   = stripLabel xs ys
stripLabel _ label = label

data RuntimeSpec = RuntimeSpec {
  -- Version of the Open Container Runtime Specification with which the bundle complies.
  ociVersion :: String,
  -- Process configures the container process.
  process :: Maybe Process,
  -- Root configures the container's root filesystem.
  root :: Maybe Root,
  -- Hostname configures the container's hostname.
  hostname :: Maybe String,
  -- Mounts configures additional mounts (on top of Root).
  mounts :: Maybe [Mount],
  -- Hooks configures callbacks for container lifecycle events.
  hooks :: Maybe [Hooks],
  -- Annotations contains arbitrary metadata for the container.
  annotations :: Maybe (Map.Map String String),
  -- Linux is platform-specific configuration for Linux based containers.
  linux :: Maybe Linux
} deriving (Show, Generic)

instance ToJSON RuntimeSpec where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON RuntimeSpec where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

defaultRuntimeSpec = RuntimeSpec {
  ociVersion = "1.0.0",
  process = Just defaultProcess,
  root = Just defaultRoot,
  hostname = Just "emergence",
  mounts = Just defaultMounts,
  hooks = Nothing,
  annotations = Nothing,
  linux = Just defaultLinux
}

-- Process contains information to start a specific application inside the container.
data Process = Process {
  -- Terminal creates an interactive terminal for the container.
  terminal :: Maybe Bool,
  -- ConsoleSize specifies the size of the console.
  consoleSize :: Maybe Box,
  -- User specifies user information for the process.
  user :: User,
  -- Args specifies the binary and arguments for the application to execute.
  args :: [String],
  -- Env populates the process environment for the process.
  env :: Maybe [String],
  -- Cwd is the current working directory for the process and must be
  -- relative to the container's root.
  cwd :: String,
  -- Capabilities are Linux capabilities that are kept for the process.
  capabilities :: Maybe LinuxCapabilities,
  -- Rlimits specifies rlimit options to apply to the process.
  rlimits :: Maybe [POSIXRlimit],
  -- NoNewPrivileges controls whether additional privileges could be gained by processes in the container.
  noNewPrivileges :: Maybe Bool,
  -- ApparmorProfile specifies the apparmor profile for the container.
  apparmorProfile :: Maybe String,
  -- Specify an oom_score_adj for the container.
  oomScoreAdj :: Maybe Int,
  -- SelinuxLabel specifies the selinux context that the container process is run as.
  selinuxLabel :: Maybe String
} deriving (Show, Generic)

instance ToJSON Process where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON Process where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

defaultProcess = Process {
  terminal = Just True,
  consoleSize = Nothing,
  user = defaultUser,
  args = ["sh"],
  env = Just [ "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
               "TERM=xterm" ],
  cwd = "/",
  capabilities = Just defaultCapabilities,
  rlimits = Just defaultRlimits,
  noNewPrivileges = Just True,
  apparmorProfile = Nothing,
  oomScoreAdj = Nothing,
  selinuxLabel = Nothing
}

-- TODO: Type capabilities instead of using string
-- LinuxCapabilities specifies the whitelist of capabilities that are kept for a process.
-- http:--man7.org/linux/man-pages/man7/capabilities.7.html
data LinuxCapabilities = LinuxCapabilities {
  -- Bounding is the set of capabilities checked by the kernel.
  bounding :: Maybe [String],
  -- Effective is the set of capabilities checked by the kernel.
  effective :: Maybe [String],
  -- Inheritable is the capabilities preserved across execve.
  inheritable :: Maybe [String],
  -- Permitted is the limiting superset for effective capabilities.
  permitted :: Maybe [String],
  -- Ambient is the ambient set of capabilities that are kept.
  ambient :: Maybe [String]
} deriving (Show, Generic)

instance ToJSON LinuxCapabilities where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON LinuxCapabilities where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

defaultCapabilities = LinuxCapabilities {
  bounding = Just [
    "CAP_AUDIT_WRITE",
    "CAP_KILL",
    "CAP_NET_BIND_SERVICE"
  ],
  permitted = Just [
    "CAP_AUDIT_WRITE",
    "CAP_KILL",
    "CAP_NET_BIND_SERVICE"
  ],
  inheritable = Just [
    "CAP_AUDIT_WRITE",
    "CAP_KILL",
    "CAP_NET_BIND_SERVICE"
  ],
  ambient = Just [
    "CAP_AUDIT_WRITE",
    "CAP_KILL",
    "CAP_NET_BIND_SERVICE"
  ],
  effective = Just [
    "CAP_AUDIT_WRITE",
    "CAP_KILL",
    "CAP_NET_BIND_SERVICE"
  ]
}

-- Box specifies dimensions of a rectangle. Used for specifying the size of a console.
data Box = Box {
  -- Height is the vertical dimension of a box.
  height :: Word,
  -- Width is the horizontal dimension of a box.
  width :: Word
} deriving (Show, Generic)

instance ToJSON Box where
instance FromJSON Box where

-- User specifies specific user (and group) information for the container process.
data User = User {
  -- UID is the user id.
  uid :: Word32,
  -- GID is the group id.
  gid :: Word32,
  -- AdditionalGids are additional group ids set for the container's process.
  additionalGids :: Maybe [Word32]
} deriving (Show, Generic)

instance ToJSON User where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

defaultUser = User {
  uid = 0,
  gid = 0,
  additionalGids = Nothing
}

-- Root contains information about the container's root filesystem on the host.
data Root = Root {
  -- Path is the absolute path to the container's root filesystem.
  path :: String,
  -- Readonly makes the root filesystem for the container readonly before the process is executed.
  readonly :: Maybe Bool
} deriving (Show, Generic)

instance ToJSON Root where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON Root where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

defaultRoot = Root {
  path = "rootfs",
  readonly = Just True
}

-- Mount specifies a mount for a container.
data Mount = Mount {
  -- Destination is the absolute path where the mount will be placed in the container.
  destination :: String,
  -- Type specifies the mount kind.
  m_type :: Maybe String,
  -- Source specifies the source path of the mount.
  source :: Maybe String,
  -- Options are fstab style mount options.
  options :: Maybe [String]
} deriving (Show, Generic)

instance ToJSON Mount where
  toJSON = genericToJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "m"
  }
instance FromJSON Mount where
  parseJSON = genericParseJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "m"
  }

defaultMounts = [ Mount { destination = "/proc"
                        , m_type = Just "proc"
                        , source = Just "proc"
                        , options = Nothing }
                , Mount { destination = "/dev"
                        , m_type = Just "tmpfs"
                        , source = Just "tmpfs"
                        , options = Just ["nosuid", "strictatime", "mode=755", "size=65536k"] }
                , Mount { destination = "/dev/pts"
                        , m_type = Just "devpts"
                        , source = Just "devpts"
                        , options = Just ["nosuid", "noexec", "newinstance", "ptmxmode=0666", "mode=0620", "gid=5"] }
                , Mount { destination = "/dev/shm"
                        , m_type = Just "tmpfs"
                        , source = Just "shm"
                        , options = Just ["nosuid", "noexec", "nodev", "mode=1777", "size=65536k"] }
                , Mount { destination = "/dev/mqueue"
                        , m_type = Just "mqueue"
                        , source = Just "mqueue"
                        , options = Just ["nosuid", "noexec", "nodev"] }
                , Mount { destination = "/sys"
                        , m_type = Just "sysfs"
                        , source = Just "sysfs"
                        , options = Just ["nosuid", "noexec", "nodev", "ro"] }
                , Mount { destination = "/sys/fs/cgroup"
                        , m_type = Just "cgroup"
                        , source = Just "cgroup"
                        , options = Just ["nosuid", "noexec", "nodev", "relatime", "ro"] } ]

-- Hook specifies a command that is run at a particular event in the lifecycle of a container
data Hook = Hook {
  h_path :: String,
  h_args :: Maybe [String],
  h_env :: Maybe [String],
  timeout :: Maybe Int
} deriving (Show, Generic)

instance ToJSON Hook where
  toJSON = genericToJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "h"
  }
instance FromJSON Hook where
  parseJSON = genericParseJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "h"
  }

-- Hooks for container setup and teardown
data Hooks = Hooks {
  -- Prestart is a list of hooks to be run before the container process is executed.
  prestart :: Maybe [Hook],
  -- Poststart is a list of hooks to be run after the container process is started.
  poststart :: Maybe [Hook],
  -- Poststop is a list of hooks to be run after the container process exits.
  poststop :: Maybe [Hook]
} deriving (Show, Generic)

instance ToJSON Hooks where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON Hooks where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

-- Linux contains platform-specific configuration for Linux based containers.
data Linux = Linux {
  -- UIDMapping specifies user mappings for supporting user namespaces.
  uidMappings :: Maybe [LinuxIDMapping],
  -- GIDMapping specifies group mappings for supporting user namespaces.
  gidMappings :: Maybe [LinuxIDMapping],
  -- Sysctl are a set of key value pairs that are set for the container on start
  sysctl :: Maybe (Map.Map String String),
  -- Resources contain cgroup information for handling resource constraints
  -- for the container
  resources :: Maybe LinuxResources,
  -- CgroupsPath specifies the path to cgroups that are created and/or joined by the container.
  -- The path is expected to be relative to the cgroups mountpoint.
  -- If resources are specified, the cgroups at CgroupsPath will be updated based on resources.
  cgroupsPath :: Maybe String,
  -- Namespaces contains the namespaces that are created and/or joined by the container
  namespaces :: Maybe [LinuxNamespace],
  -- Devices are a list of device nodes that are created for the container
  devices :: Maybe [LinuxDevice],
  -- Seccomp specifies the seccomp security settings for the container.
  seccomp :: Maybe LinuxSeccomp,
  -- RootfsPropagation is the rootfs mount propagation mode for the container.
  rootfsPropagation :: Maybe String,
  -- MaskedPaths masks over the provided paths inside the container.
  maskedPaths :: Maybe [String],
  -- ReadonlyPaths sets the provided paths as RO inside the container.
  readonlyPaths :: Maybe [String],
  -- MountLabel specifies the selinux context for the mounts in the container.
  mountLabel :: Maybe String,
  -- IntelRdt contains Intel Resource Director Technology (RDT) information for
  -- handling resource constraints (e.g., L3 cache, memory bandwidth) for the container
  intelRdt :: Maybe LinuxIntelRdt
} deriving (Show, Generic)

instance ToJSON Linux where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON Linux where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

defaultLinux = Linux { uidMappings = Nothing
                     , gidMappings = Nothing
                     , sysctl = Nothing
                     , resources = Just defaultLinuxResources
                     , cgroupsPath = Nothing
                     , namespaces = Just defaultLinuxNamespace
                     , devices = Nothing
                     , seccomp = Nothing
                     , rootfsPropagation = Nothing
                     , maskedPaths = Just [ "/proc/kcore"
                                          , "/proc/latency_stats"
                                          , "/proc/timer_list"
                                          , "/proc/timer_stats"
                                          , "/proc/sched_debug"
                                          , "/sys/firmware"
                                          , "/proc/scsi" ]
                     , readonlyPaths = Just [ "/proc/asound"
                                            , "/proc/bus"
                                            , "/proc/fs"
                                            , "/proc/irq"
                                            , "/proc/sys"
                                            , "/proc/sysrq-trigger"]
                     , mountLabel = Nothing 
                     , intelRdt = Nothing }

type LinuxNamespaceType = String

-- LinuxNamespace is the configuration for a Linux namespace
data LinuxNamespace = LinuxNamespace {
  -- Type is the type of namespace
  ns_type :: LinuxNamespaceType,
  -- Path is a path to an existing namespace persisted on disk that can be joined
  -- and is of the same type
  ns_path :: Maybe String
} deriving (Show, Generic)

instance ToJSON LinuxNamespace where
  toJSON = genericToJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "ns"
  }
instance FromJSON LinuxNamespace where
  parseJSON = genericParseJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "ns"
  }

defaultLinuxNamespace = [ LinuxNamespace "pid" Nothing
                        , LinuxNamespace "network" Nothing
                        , LinuxNamespace "ipc" Nothing
                        , LinuxNamespace "uts" Nothing
                        , LinuxNamespace "mount" Nothing ]

-- data Namespace = PIDNamespace 
--                | NetworkNamespace 
--                | MountNamespace
--                | IPCNamespace 
--                | UTSNamespace 
--                | UserNamespace 
--                | CgroupNamespace
--                  deriving (Show, Generic)

-- instance ToJSON Namespace where 
--   toJSON PIDNamespace = "pid"
--   toJSON NetworkNamespace = "network"
--   toJSON MountNamespace = "mount"
--   toJSON IPCNamespace = "ipc"
--   toJSON UTSNamespace = "uts"
--   toJSON UserNamespace = "user"
--   toJSON CgroupNamespace = "cgroup"
-- instance FromJSON Namespace where 
--   parseJSON = withText "namespace" (
--                 \case
--                   "pid" -> return PIDNamespace
--                   "network" -> return NetworkNamespace
--                   "mount" -> return MountNamespace
--                   "ipc" -> return IPCNamespace
--                   "uts" -> return UTSNamespace
--                   "user" -> return UserNamespace
--                   "cgroup" -> return CgroupNamespace)

-- const (
--   -- PIDNamespace for isolating process IDs
--   PIDNamespace LinuxNamespaceType = "pid"
--   -- NetworkNamespace for isolating network devices, stacks, ports, etc
--   NetworkNamespace = "network"
--   -- MountNamespace for isolating mount points
--   MountNamespace = "mount"
--   -- IPCNamespace for isolating System V IPC, POSIX message queues
--   IPCNamespace = "ipc"
--   -- UTSNamespace for isolating hostname and NIS domain name
--   UTSNamespace = "uts"
--   -- UserNamespace for isolating user and group IDs
--   UserNamespace = "user"
--   -- CgroupNamespace for isolating cgroup hierarchies
--   CgroupNamespace = "cgroup"
-- )

-- LinuxIDMapping specifies UID/GID mappings
data LinuxIDMapping = LinuxIDMapping {
  -- ContainerID is the starting UID/GID in the container
  containerID :: Word32,
  -- HostID is the starting UID/GID on the host to be mapped to 'ContainerID'
  hostID :: Word32,
  -- Size is the number of IDs to be mapped
  size :: Word32
} deriving (Show, Generic)

instance ToJSON LinuxIDMapping where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON LinuxIDMapping where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

-- POSIXRlimit type and restrictions
data POSIXRlimit = POSIXRlimit {
  -- Type of the rlimit to set
  rl_type :: String,
  -- Hard is the hard limit for the specified type
  hard :: Word64,
  -- Soft is the soft limit for the specified type
  soft :: Word64
} deriving (Show, Generic)

instance ToJSON POSIXRlimit where
  toJSON = genericToJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "rl"
  }
instance FromJSON POSIXRlimit where
  parseJSON = genericParseJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "rl"
  }

defaultRlimits = [ POSIXRlimit "RLIMIT_NOFILE" 1024 1024 ]

-- LinuxHugepageLimit structure corresponds to limiting kernel hugepages
data LinuxHugepageLimit = LinuxHugepageLimit {
  -- Pagesize is the hugepage size
  pagesize :: String,
  -- Limit is the limit of "hugepagesize" hugetlb usage
  limit :: Word64
} deriving (Show, Generic)

instance ToJSON LinuxHugepageLimit where
instance FromJSON LinuxHugepageLimit where

-- LinuxInterfacePriority for network interfaces
data LinuxInterfacePriority = LinuxInterfacePriority {
  -- Name is the name of the network interface
  name :: String,
  -- Priority for the interface
  priority :: Word32
} deriving (Show, Generic)

instance ToJSON LinuxInterfacePriority where
instance FromJSON LinuxInterfacePriority where

-- linuxBlockIODevice holds major:minor format supported in blkio cgroup
-- LinuxWeightDevice struct holds a `major:minor weight` pair for weightDevice
data LinuxWeightDevice = LinuxWeightDevice { 
  wd_major :: Int64, 
  wd_minor :: Int64, 
  wd_weight :: Maybe Word16,
  wd_leafWeight :: Maybe Word16 
} deriving (Show, Generic)

instance ToJSON LinuxWeightDevice where
  toJSON = genericToJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "wd"
  }
instance FromJSON LinuxWeightDevice where
  parseJSON = genericParseJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "wd"
  }

-- LinuxThrottleDevice struct holds a `major:minor rate_per_second` pair
data LinuxThrottleDevice = LinuxThrottleDevice { 
  td_major :: Int64, 
  td_minor :: Int64, 
  td_rate :: Word64 
} deriving (Show, Generic)

instance ToJSON LinuxThrottleDevice where
  toJSON = genericToJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "wd"
  }
instance FromJSON LinuxThrottleDevice where
  parseJSON = genericParseJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "wd"
  }

-- LinuxBlockIO for Linux cgroup 'blkio' resource management
data LinuxBlockIO = LinuxBlockIO {
  -- Specifies per cgroup weight
  weight :: Maybe Word16 ,
  -- Specifies tasks' weight in the given cgroup while competing with the cgroup's child cgroups, CFQ scheduler only
  leafWeight :: Maybe Word16,
  -- Weight per cgroup per device, can override BlkioWeight
  weightDevice :: Maybe [LinuxWeightDevice],
  -- IO read rate limit per cgroup per device, bytes per second
  throttleReadBpsDevice :: Maybe [LinuxThrottleDevice],
  -- IO write rate limit per cgroup per device, bytes per second
  throttleWriteBpsDevice :: Maybe [LinuxThrottleDevice],
  -- IO read rate limit per cgroup per device, IO per second
  throttleReadIOPSDevice :: Maybe [LinuxThrottleDevice],
  -- IO write rate limit per cgroup per device, IO per second
  throttleWriteIOPSDevice :: Maybe [LinuxThrottleDevice]
} deriving (Show, Generic)

instance ToJSON LinuxBlockIO where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON LinuxBlockIO where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

-- LinuxMemory for Linux cgroup 'memory' resource management
data LinuxMemory = LinuxMemory {
  -- Memory limit (in bytes).
  mem_limit :: Maybe Int64,
  -- Memory reservation or soft_limit (in bytes).
  reservation :: Maybe Int64,
  -- Total memory limit (memory + swap).
  swap :: Maybe Int64,
  -- Kernel memory limit (in bytes).
  kernel :: Maybe Int64,
  -- Kernel memory limit for tcp (in bytes)
  kernelTCP :: Maybe Int64,
  -- How aggressive the kernel will swap memory pages.
  swappiness :: Maybe Int64,
  -- DisableOOMKiller disables the OOM killer for out of memory conditions
  disableOOMKiller :: Maybe Bool
} deriving (Show, Generic)

instance ToJSON LinuxMemory where
  toJSON = genericToJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "mem"
  }
instance FromJSON LinuxMemory where
  parseJSON = genericParseJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "mem"
  }


-- LinuxCPU for Linux cgroup 'cpu' resource management
data LinuxCPU = LinuxCPU {
  -- CPU shares (relative weight (ratio) vs. other cgroups with cpu shares).
  shares :: Maybe Word64,
  -- CPU hardcap limit (in usecs). Allowed cpu time in a given period.
  quota :: Maybe Int64,
  -- CPU period to be used for hardcapping (in usecs).
  period :: Maybe Word64,
  -- How much time realtime scheduling may use (in usecs).
  realtimeRuntime :: Maybe Int64,
  -- CPU period to be used for realtime scheduling (in usecs).
  realtimePeriod :: Maybe Word64,
  -- CPUs to use within the cpuset. Default is to use any CPU available.
  cpus :: Maybe String,
  -- List of memory nodes in the cpuset. Default is to use any available memory node.
  mems :: Maybe String
} deriving (Show, Generic)

instance ToJSON LinuxCPU where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON LinuxCPU where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

-- LinuxPids for Linux cgroup 'pids' resource management (Linux 4.3)
data LinuxPids = LinuxPids {
  -- Maximum number of PIDs. Default is "no limit".
  pid_limit :: Int64
} deriving (Show, Generic)

instance ToJSON LinuxPids where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = stripLabel "pid"
  }
instance FromJSON LinuxPids where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = stripLabel "pid"
  }

-- LinuxNetwork identification and priority configuration
data LinuxNetwork = LinuxNetwork {
  -- Set class identifier for container's network packets
  classID :: Maybe Word32,
  -- Set priority of network traffic for container
  priorities :: Maybe [LinuxInterfacePriority]
} deriving (Show, Generic)

instance ToJSON LinuxNetwork where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON LinuxNetwork where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

-- LinuxRdma for Linux cgroup 'rdma' resource management (Linux 4.11)
data LinuxRdma = LinuxRdma {
  -- Maximum number of HCA handles that can be opened. Default is "no limit".
  hcaHandles :: Maybe Word32,
  -- Maximum number of HCA objects that can be created. Default is "no limit".
  hcaObjects :: Maybe Word32
} deriving (Show, Generic)

instance ToJSON LinuxRdma where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON LinuxRdma where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

-- LinuxResources has container runtime resource constraints
data LinuxResources = LinuxResources {
  -- Devices configures the device whitelist.
  r_devices :: Maybe [LinuxDeviceCgroup],
  -- Memory restriction configuration
  memory :: Maybe LinuxMemory,
  -- CPU resource restriction configuration
  cpu :: Maybe LinuxCPU,
  -- Task resource restriction configuration.
  pids :: Maybe LinuxPids,
  -- BlockIO restriction configuration
  blockIO :: Maybe LinuxBlockIO,
  -- Hugetlb limit (in bytes)
  hugepageLimits :: Maybe [LinuxHugepageLimit],
  -- Network restriction configuration
  network :: Maybe LinuxNetwork,
  -- Rdma resource restriction configuration.
  -- Limits are a set of key value pairs that define RDMA resource limits,
  -- where the key is device name and value is resource limits.
  rdma :: Maybe (Map.Map String LinuxRdma)
} deriving (Show, Generic)

instance ToJSON LinuxResources where
  toJSON = genericToJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "r"
  }
instance FromJSON LinuxResources where
  parseJSON = genericParseJSON defaultOptions { 
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "r" 
  }

defaultLinuxResources = LinuxResources {
  r_devices = Just [LinuxDeviceCgroup {
    allow = False,
    devcg_type = Nothing,
    devcg_major = Nothing,
    devcg_minor = Nothing,
    access = Just "rwm"
  }],
  memory = Nothing,
  cpu = Nothing,
  pids = Nothing,
  blockIO = Nothing,
  hugepageLimits = Nothing,
  network = Nothing,
  rdma = Nothing
}

-- LinuxDevice represents the mknod information for a Linux special device file
data LinuxDevice = LinuxDevice {
  -- Path to the device.
  dev_path :: String,
  -- Device type, block, char, etc.
  dev_type :: String,
  -- Major is the device's major number.
  dev_Major :: Int64,
  -- Minor is the device's minor number.
  dev_Minor :: Int64,
  -- FileMode permission bits for the device.
  fileMode :: Maybe Word32,
  -- UID of the device.
  dev_uid :: Maybe Word32,
  -- Gid of the device.
  dev_gid :: Maybe Word32
} deriving (Show, Generic)

instance ToJSON LinuxDevice where
  toJSON = genericToJSON defaultOptions {
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "dev"
  }
instance FromJSON LinuxDevice where
  parseJSON = genericParseJSON defaultOptions {
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "dev"
  }

-- LinuxDeviceCgroup represents a device rule for the whitelist controller
data LinuxDeviceCgroup = LinuxDeviceCgroup {
  -- Allow or deny
  allow :: Bool,
  -- Device type, block, char, etc.
  devcg_type :: Maybe String,
  -- Major is the device's major number.
  devcg_major :: Maybe Int64,
  -- Minor is the device's minor number.
  devcg_minor :: Maybe Int64,
  -- Cgroup access permissions format, rwm.
  access :: Maybe String
} deriving (Show, Generic)

instance ToJSON LinuxDeviceCgroup where
  toJSON = genericToJSON defaultOptions {
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "devcg"
  }
instance FromJSON LinuxDeviceCgroup where
  parseJSON = genericParseJSON defaultOptions {
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "devcg"
  }


-- LinuxSeccomp represents syscall restrictions
data LinuxSeccomp = LinuxSeccomp {
  defaultAction :: LinuxSeccompAction,
  architectures :: Maybe [Arch],
  syscalls :: Maybe [LinuxSyscall]
} deriving (Show, Generic)

instance ToJSON LinuxSeccomp where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON LinuxSeccomp where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

-- Arch used for additional architectures
type Arch = String

-- Additional architectures permitted to be used for system calls
-- By default only the native architecture of the kernel is permitted
-- const (
--   ArchX86     Arch = "SCMP_ARCH_X86"
--   ArchX86_64    Arch = "SCMP_ARCH_X86_64"
--   ArchX32     Arch = "SCMP_ARCH_X32"
--   ArchARM     Arch = "SCMP_ARCH_ARM"
--   ArchAARCH64   Arch = "SCMP_ARCH_AARCH64"
--   ArchMIPS    Arch = "SCMP_ARCH_MIPS"
--   ArchMIPS64    Arch = "SCMP_ARCH_MIPS64"
--   ArchMIPS64N32   Arch = "SCMP_ARCH_MIPS64N32"
--   ArchMIPSEL    Arch = "SCMP_ARCH_MIPSEL"
--   ArchMIPSEL64  Arch = "SCMP_ARCH_MIPSEL64"
--   ArchMIPSEL64N32 Arch = "SCMP_ARCH_MIPSEL64N32"
--   ArchPPC     Arch = "SCMP_ARCH_PPC"
--   ArchPPC64     Arch = "SCMP_ARCH_PPC64"
--   ArchPPC64LE   Arch = "SCMP_ARCH_PPC64LE"
--   ArchS390    Arch = "SCMP_ARCH_S390"
--   ArchS390X     Arch = "SCMP_ARCH_S390X"
--   ArchPARISC    Arch = "SCMP_ARCH_PARISC"
--   ArchPARISC64  Arch = "SCMP_ARCH_PARISC64"
-- )

-- LinuxSeccompAction taken upon Seccomp rule match
type LinuxSeccompAction = String

-- Define actions for Seccomp rules
-- const (
--   ActKill  LinuxSeccompAction = "SCMP_ACT_KILL"
--   ActTrap  LinuxSeccompAction = "SCMP_ACT_TRAP"
--   ActErrno LinuxSeccompAction = "SCMP_ACT_ERRNO"
--   ActTrace LinuxSeccompAction = "SCMP_ACT_TRACE"
--   ActAllow LinuxSeccompAction = "SCMP_ACT_ALLOW"
-- )

-- LinuxSeccompOperator used to match syscall arguments in Seccomp
type LinuxSeccompOperator = String

-- Define operators for syscall arguments in Seccomp
-- const (
--   OpNotEqual   LinuxSeccompOperator = "SCMP_CMP_NE"
--   OpLessThan   LinuxSeccompOperator = "SCMP_CMP_LT"
--   OpLessEqual  LinuxSeccompOperator = "SCMP_CMP_LE"
--   OpEqualTo    LinuxSeccompOperator = "SCMP_CMP_EQ"
--   OpGreaterEqual LinuxSeccompOperator = "SCMP_CMP_GE"
--   OpGreaterThan  LinuxSeccompOperator = "SCMP_CMP_GT"
--   OpMaskedEqual  LinuxSeccompOperator = "SCMP_CMP_MASKED_EQ"
-- )

-- LinuxSeccompArg used for matching specific syscall arguments in Seccomp
data LinuxSeccompArg = LinuxSeccompArg {
  index :: Word,
  value :: Int,
  valueTwo :: Maybe Int,
  op :: LinuxSeccompOperator
} deriving (Show, Generic)

instance ToJSON LinuxSeccompArg where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON LinuxSeccompArg where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

-- LinuxSyscall is used to match a syscall in Seccomp
data LinuxSyscall = LinuxSyscall {
  names :: [String],
  action :: LinuxSeccompAction,
  sys_args :: Maybe [LinuxSeccompArg]
} deriving (Show, Generic)

instance ToJSON LinuxSyscall where
  toJSON = genericToJSON defaultOptions {
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "sys"
  }
instance FromJSON LinuxSyscall where
  parseJSON = genericParseJSON defaultOptions {
    omitNothingFields = True,
    fieldLabelModifier = stripLabel "sys"
  }

-- LinuxIntelRdt has container runtime resource constraints for Intel RDT
-- CAT and MBA features which introduced in Linux 4.10 and 4.12 kernel
data LinuxIntelRdt = LinuxIntelRdt {
  -- The identity for RDT Class of Service
  closID :: Maybe String,
  -- The schema for L3 cache id and capacity bitmask (CBM)
  -- Format: "L3:<cache_id0>=<cbm0>;<cache_id1>=<cbm1>;..."
  l3CacheSchema :: Maybe String,
  -- The schema of memory bandwidth per L3 cache id
  -- Format: "MB:<cache_id0>=bandwidth0;<cache_id1>=bandwidth1;..."
  -- The unit of memory bandwidth is specified in "percentages" by
  -- default, and in "MBps" if MBA Software Controller is enabled.
  memBwSchema :: Maybe String
} deriving (Show, Generic)

instance ToJSON LinuxIntelRdt where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON LinuxIntelRdt where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

-- runtimeSpec :: ImageSpec -> RuntimeSpec
-- runtimeSpec c path = 
