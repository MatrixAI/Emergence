{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Runtime.Spec where

import GHC.Generics
import Data.Aeson 
import Data.Word
import qualified Data.Map as Map

stripLabel :: String -> String -> String
stripLabel (x:xs) (y:ys) 
  | y == '_' = ys
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
  hostName :: Maybe String,
  -- Mounts configures additional mounts (on top of Root).
  mounts :: Maybe [Mount],
  -- Hooks configures callbacks for container lifecycle events.
  -- hooks :: Maybe [Hooks],
  -- Annotations contains arbitrary metadata for the container.
  annotations :: Maybe (Map.Map String String)
  -- Linux is platform-specific configuration for Linux based containers.
  -- linuxSpec :: Maybe LinuxSpec
} deriving (Show, Generic)

instance ToJSON RuntimeSpec where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON RuntimeSpec where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

defaultRuntimeSpec = RuntimeSpec {
  ociVersion = "1.0.1-dev",
  process = Just defaultProcess,
  root = Just defaultRoot,
  hostName = Just "emergence",
  annotations = Nothing
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
  -- rlimits :: Maybe [POSIXRlimit],
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
  -- rlimits = Just defaultRlimits,
  noNewPrivileges = Just True,
  apparmorProfile = Nothing,
  oomScoreAdj = Nothing,
  selinuxLabel = Nothing
}

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
  gid = 0
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

defaultMounts = [
  Mount {
    destination = "/proc",
    m_type = Just "proc",
    source = Just "proc",
    options = Nothing
  },
  Mount {
    destination = "/dev",
    m_type = Just "tmpfs",
    source = Just "tmpfs",
    options = Just ["nosuid", "strictatime", "mode=755", "size=65536k"]
  },
  Mount {
    destination = "/dev/pts",
    m_type = Just "devpts",
    source = Just "devpts",
    options = Just ["nosuid", "noexec", "newinstance", "ptmxmode=0666", "mode=0620", "gid=5"]
  },
  Mount {
    destination = "/dev/shm",
    m_type = Just "tmpfs",
    source = Just "shm",
    options = Just ["nosuid", "noexec", "nodev", "mode=1777", "size=65536k"]
  },
  Mount {
    destination = "/dev/mqueue",
    m_type = Just "mqueue",
    source = Just "mqueue",
    options = Just ["nosuid", "noexec", "nodev"]
  },
  Mount {
    destination = "/sys",
    m_type = Just "sysfs",
    source = Just "sysfs",
    options = Just ["nosuid", "noexec", "nodev", "ro"]
  },
  Mount {
    destination = "/sys/fs/cgroup",
    m_type = Just "cgroup",
    source = Just "cgroup",
    options = Just ["nosuid", "noexec", "nodev", "relatime", "ro"]
  }
]

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
{-
data LinuxSpec = LinuxSpec {
  -- UIDMapping specifies user mappings for supporting user namespaces.
  uidMappings :: [LinuxIDMapping]
  -- GIDMapping specifies group mappings for supporting user namespaces.
  gidMappings :: [LinuxIDMapping]
  -- Sysctl are a set of key value pairs that are set for the container on start
  sysctl :: Map String String
  -- Resources contain cgroup information for handling resource constraints
  -- for the container
  resources :: LinuxResources
  -- CgroupsPath specifies the path to cgroups that are created and/or joined by the container.
  -- The path is expected to be relative to the cgroups mountpoint.
  -- If resources are specified, the cgroups at CgroupsPath will be updated based on resources.
  cgroupsPath :: String
  -- Namespaces contains the namespaces that are created and/or joined by the container
  namespaces :: [LinuxNamespace]
  -- Devices are a list of device nodes that are created for the container
  devices :: [LinuxDevice]
  -- Seccomp specifies the seccomp security settings for the container.
  seccomp :: LinuxSeccomp
  -- RootfsPropagation is the rootfs mount propagation mode for the container.
  rootfsPropagation :: String
  -- MaskedPaths masks over the provided paths inside the container.
  maskedPaths :: [String]
  -- ReadonlyPaths sets the provided paths as RO inside the container.
  readonlyPaths :: [String]
  -- MountLabel specifies the selinux context for the mounts in the container.
  mountLabel :: [String]
  -- IntelRdt contains Intel Resource Director Technology (RDT) information
  -- for handling resource constraints (e.g., L3 cache) for the container
  intelRdt :: LinuxIntelRdt
}

-- Linux contains platform-specific configuration for Linux based containers.
data Linux = Linux {
  -- UIDMapping specifies user mappings for supporting user namespaces.
  uidMappings :: [LinuxIDMapping]
  -- GIDMapping specifies group mappings for supporting user namespaces.
  gidMappings :: [LinuxIDMapping]
  -- Sysctl are a set of key value pairs that are set for the container on start
  sysctl :: Map String String
  -- Resources contain cgroup information for handling resource constraints
  -- for the container
  resources :: LinuxResources
  -- CgroupsPath specifies the path to cgroups that are created and/or joined by the container.
  -- The path is expected to be relative to the cgroups mountpoint.
  -- If resources are specified, the cgroups at CgroupsPath will be updated based on resources.
  cgroupsPath :: String
  -- Namespaces contains the namespaces that are created and/or joined by the container
  namespaces :: [LinuxNamespace]
  -- Devices are a list of device nodes that are created for the container
  devices :: [LinuxDevice]
  -- Seccomp specifies the seccomp security settings for the container.
  seccomp :: LinuxSeccomp
  -- RootfsPropagation is the rootfs mount propagation mode for the container.
  rootfsPropagation :: String
  -- MaskedPaths masks over the provided paths inside the container.
  maskedPaths :: [String]
  -- ReadonlyPaths sets the provided paths as RO inside the container.
  readonlyPaths :: [String]
  -- MountLabel specifies the selinux context for the mounts in the container.
  mountLabel :: String
  -- IntelRdt contains Intel Resource Director Technology (RDT) information for
  -- handling resource constraints (e.g., L3 cache, memory bandwidth) for the container
  intelRdt :: LinuxIntelRdt
}

-- LinuxNamespace is the configuration for a Linux namespace
data LinuxNamespace = LinuxNamespace {
  -- Type is the type of namespace
  type :: LinuxNamespaceType
  -- Path is a path to an existing namespace persisted on disk that can be joined
  -- and is of the same type
  path :: String
}

-- LinuxNamespaceType is one of the Linux namespaces
type LinuxNamespaceType String

const (
  -- PIDNamespace for isolating process IDs
  PIDNamespace LinuxNamespaceType = "pid"
  -- NetworkNamespace for isolating network devices, stacks, ports, etc
  NetworkNamespace = "network"
  -- MountNamespace for isolating mount points
  MountNamespace = "mount"
  -- IPCNamespace for isolating System V IPC, POSIX message queues
  IPCNamespace = "ipc"
  -- UTSNamespace for isolating hostname and NIS domain name
  UTSNamespace = "uts"
  -- UserNamespace for isolating user and group IDs
  UserNamespace = "user"
  -- CgroupNamespace for isolating cgroup hierarchies
  CgroupNamespace = "cgroup"
)

-- LinuxIDMapping specifies UID/GID mappings
data LinuxIDMapping = LinuxIDMapping {
  -- ContainerID is the starting UID/GID in the container
  containerID :: Int
  -- HostID is the starting UID/GID on the host to be mapped to 'ContainerID'
  hostID :: Int
  -- Size is the number of IDs to be mapped
  size :: Int
}

-- POSIXRlimit type and restrictions
data POSIXRlimit = POSIXRlimit {
  -- Type of the rlimit to set
  type :: String
  -- Hard is the hard limit for the specified type
  hard :: Int
  -- Soft is the soft limit for the specified type
  soft :: Int
}

-- LinuxHugepageLimit structure corresponds to limiting kernel hugepages
data LinuxHugepageLimit = LinuxHugepageLimit {
  -- Pagesize is the hugepage size
  pagesize :: String
  -- Limit is the limit of "hugepagesize" hugetlb usage
  limit :: Int
}

-- LinuxInterfacePriority for network interfaces
data LinuxInterfacePriority = LinuxInterfacePriority {
  -- Name is the name of the network interface
  name :: String
  -- Priority for the interface
  priority :: Int
}

-- linuxBlockIODevice holds major:minor format supported in blkio cgroup
data linuxBlockIODevice = linuxBlockIODevice {
  -- Major is the device's major number.
  Major :: Int
  -- Minor is the device's minor number.
  Minor :: Int
}

-- LinuxWeightDevice struct holds a `major:minor weight` pair for weightDevice
data LinuxWeightDevice = LinuxWeightDevice {
  linuxBlockIODevice
  -- Weight is the bandwidth rate for the device.
  Weight :: Int
  -- LeafWeight is the bandwidth rate for the device while competing with the cgroup's child cgroups, CFQ scheduler only
  LeafWeight :: Int
}

-- LinuxThrottleDevice struct holds a `major:minor rate_per_second` pair
data LinuxThrottleDevice = LinuxThrottleDevice {
  linuxBlockIODevice
  -- Rate is the IO rate limit per cgroup per device
  Rate :: Int
}
-- LinuxBlockIO for Linux cgroup 'blkio' resource management
data LinuxBlockIO = LinuxBlockIO {
  -- Specifies per cgroup weight
  Weight :: Int
  -- Specifies tasks' weight in the given cgroup while competing with the cgroup's child cgroups, CFQ scheduler only
  LeafWeight :: Int
  -- Weight per cgroup per device, can override BlkioWeight
  WeightDevice :: [LinuxWeightDevice]
  -- IO read rate limit per cgroup per device, bytes per second
  ThrottleReadBpsDevice :: [LinuxThrottleDevice]
  -- IO write rate limit per cgroup per device, bytes per second
  ThrottleWriteBpsDevice :: [LinuxThrottleDevice]
  -- IO read rate limit per cgroup per device, IO per second
  ThrottleReadIOPSDevice :: [LinuxThrottleDevice]
  -- IO write rate limit per cgroup per device, IO per second
  ThrottleWriteIOPSDevice :: [LinuxThrottleDevice]
}

-- LinuxMemory for Linux cgroup 'memory' resource management
data LinuxMemory = LinuxMemory {
  -- Memory limit (in bytes).
  Limit :: Int
  -- Memory reservation or soft_limit (in bytes).
  Reservation :: Int
  -- Total memory limit (memory + swap).
  Swap :: Int
  -- Kernel memory limit (in bytes).
  Kernel :: Int
  -- Kernel memory limit for tcp (in bytes)
  KernelTCP :: Int
  -- How aggressive the kernel will swap memory pages.
  Swappiness :: Int
  -- DisableOOMKiller disables the OOM killer for out of memory conditions
  DisableOOMKiller :: Bool
}

-- LinuxCPU for Linux cgroup 'cpu' resource management
data LinuxCPU = LinuxCPU {
  -- CPU shares (relative weight (ratio) vs. other cgroups with cpu shares).
  Shares :: Int
  -- CPU hardcap limit (in usecs). Allowed cpu time in a given period.
  Quota :: Int
  -- CPU period to be used for hardcapping (in usecs).
  Period :: Int
  -- How much time realtime scheduling may use (in usecs).
  RealtimeRuntime :: Int
  -- CPU period to be used for realtime scheduling (in usecs).
  RealtimePeriod :: Int
  -- CPUs to use within the cpuset. Default is to use any CPU available.
  Cpus :: String
  -- List of memory nodes in the cpuset. Default is to use any available memory node.
  Mems :: String
}

-- LinuxPids for Linux cgroup 'pids' resource management (Linux 4.3)
data LinuxPids = LinuxPids {
  -- Maximum number of PIDs. Default is "no limit".
  Limit :: Int
}

-- LinuxNetwork identification and priority configuration
data LinuxNetwork = LinuxNetwork {
  -- Set class identifier for container's network packets
  ClassID :: Int
  -- Set priority of network traffic for container
  Priorities :: [LinuxInterfacePriority]
}

-- LinuxRdma for Linux cgroup 'rdma' resource management (Linux 4.11)
data LinuxRdma = LinuxRdma {
  -- Maximum number of HCA handles that can be opened. Default is "no limit".
  HcaHandles :: Int
  -- Maximum number of HCA objects that can be created. Default is "no limit".
  HcaObjects :: Int
}

-- LinuxResources has container runtime resource constraints
data LinuxResources = LinuxResources {
  -- Devices configures the device whitelist.
  Devices :: [LinuxDeviceCgroup]
  -- Memory restriction configuration
  Memory :: LinuxMemory
  -- CPU resource restriction configuration
  CPU :: LinuxCPU
  -- Task resource restriction configuration.
  Pids :: LinuxPids
  -- BlockIO restriction configuration
  BlockIO :: LinuxBlockIO
  -- Hugetlb limit (in bytes)
  HugepageLimits :: [LinuxHugepageLimit]
  -- Network restriction configuration
  Network :: LinuxNetwork
  -- Rdma resource restriction configuration.
  -- Limits are a set of key value pairs that define RDMA resource limits,
  -- where the key is device name and value is resource limits.
  Rdma :: Map String LinuxRdma
}

-- LinuxDevice represents the mknod information for a Linux special device file
data LinuxDevice = LinuxDevice {
  -- Path to the device.
  Path :: String
  -- Device type, block, char, etc.
  Type :: String
  -- Major is the device's major number.
  Major :: Int
  -- Minor is the device's minor number.
  Minor :: Int
  -- FileMode permission bits for the device.
  FileMode *os.FileMode
  -- UID of the device.
  UID :: Int
  -- Gid of the device.
  GID :: Int
}

-- LinuxDeviceCgroup represents a device rule for the whitelist controller
data LinuxDeviceCgroup = LinuxDeviceCgroup {
  -- Allow or deny
  Allow :: Bool
  -- Device type, block, char, etc.
  Type :: String
  -- Major is the device's major number.
  Major :: Int
  -- Minor is the device's minor number.
  Minor :: Int
  -- Cgroup access permissions format, rwm.
  Access :: String
}

-- LinuxSeccomp represents syscall restrictions
data LinuxSeccomp = LinuxSeccomp {
  DefaultAction :: LinuxSeccompAction
  Architectures []Arch       `json:"architectures,omitempty"`
  Syscalls    []LinuxSyscall   `json:"syscalls,omitempty"`
}

-- Arch used for additional architectures
type Arch String

-- Additional architectures permitted to be used for system calls
-- By default only the native architecture of the kernel is permitted
const (
  ArchX86     Arch = "SCMP_ARCH_X86"
  ArchX86_64    Arch = "SCMP_ARCH_X86_64"
  ArchX32     Arch = "SCMP_ARCH_X32"
  ArchARM     Arch = "SCMP_ARCH_ARM"
  ArchAARCH64   Arch = "SCMP_ARCH_AARCH64"
  ArchMIPS    Arch = "SCMP_ARCH_MIPS"
  ArchMIPS64    Arch = "SCMP_ARCH_MIPS64"
  ArchMIPS64N32   Arch = "SCMP_ARCH_MIPS64N32"
  ArchMIPSEL    Arch = "SCMP_ARCH_MIPSEL"
  ArchMIPSEL64  Arch = "SCMP_ARCH_MIPSEL64"
  ArchMIPSEL64N32 Arch = "SCMP_ARCH_MIPSEL64N32"
  ArchPPC     Arch = "SCMP_ARCH_PPC"
  ArchPPC64     Arch = "SCMP_ARCH_PPC64"
  ArchPPC64LE   Arch = "SCMP_ARCH_PPC64LE"
  ArchS390    Arch = "SCMP_ARCH_S390"
  ArchS390X     Arch = "SCMP_ARCH_S390X"
  ArchPARISC    Arch = "SCMP_ARCH_PARISC"
  ArchPARISC64  Arch = "SCMP_ARCH_PARISC64"
)

-- LinuxSeccompAction taken upon Seccomp rule match
type LinuxSeccompAction String

-- Define actions for Seccomp rules
const (
  ActKill  LinuxSeccompAction = "SCMP_ACT_KILL"
  ActTrap  LinuxSeccompAction = "SCMP_ACT_TRAP"
  ActErrno LinuxSeccompAction = "SCMP_ACT_ERRNO"
  ActTrace LinuxSeccompAction = "SCMP_ACT_TRACE"
  ActAllow LinuxSeccompAction = "SCMP_ACT_ALLOW"
)

-- LinuxSeccompOperator used to match syscall arguments in Seccomp
type LinuxSeccompOperator String

-- Define operators for syscall arguments in Seccomp
const (
  OpNotEqual   LinuxSeccompOperator = "SCMP_CMP_NE"
  OpLessThan   LinuxSeccompOperator = "SCMP_CMP_LT"
  OpLessEqual  LinuxSeccompOperator = "SCMP_CMP_LE"
  OpEqualTo    LinuxSeccompOperator = "SCMP_CMP_EQ"
  OpGreaterEqual LinuxSeccompOperator = "SCMP_CMP_GE"
  OpGreaterThan  LinuxSeccompOperator = "SCMP_CMP_GT"
  OpMaskedEqual  LinuxSeccompOperator = "SCMP_CMP_MASKED_EQ"
)

-- LinuxSeccompArg used for matching specific syscall arguments in Seccomp
data LinuxSeccompArg = LinuxSeccompArg {
  Index  uint         `json:"index"`
  Value  Int         `json:"value"`
  ValueTwo Int         `json:"valueTwo,omitempty"`
  Op     LinuxSeccompOperator `json:"op"`
}

-- LinuxSyscall is used to match a syscall in Seccomp
data LinuxSyscall = LinuxSyscall {
  Names  []String       `json:"names"`
  Action :: LinuxSeccompAction
  Args   []LinuxSeccompArg  `json:"args,omitempty"`
}

-- LinuxIntelRdt has container runtime resource constraints for Intel RDT
-- CAT and MBA features which introduced in Linux 4.10 and 4.12 kernel
data LinuxIntelRdt = LinuxIntelRdt {
  -- The identity for RDT Class of Service
  ClosID :: String
  -- The schema for L3 cache id and capacity bitmask (CBM)
  -- Format: "L3:<cache_id0>=<cbm0>;<cache_id1>=<cbm1>;..."
  L3CacheSchema :: String

  -- The schema of memory bandwidth per L3 cache id
  -- Format: "MB:<cache_id0>=bandwidth0;<cache_id1>=bandwidth1;..."
  -- The unit of memory bandwidth is specified in "percentages" by
  -- default, and in "MBps" if MBA Software Controller is enabled.
  MemBwSchema :: String
}

runtimeSpec :: ImageConfig -> String -> RuntimeSpec
runtimeSpec c path = 

-}