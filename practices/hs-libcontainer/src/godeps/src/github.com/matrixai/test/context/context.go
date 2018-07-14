package context

// Base defines information used for any commands
type Base interface {
	// Root directory for storage of container state (should be in tmpfs)
	Root() string
	// Criu path to the briu binary
	Criu() string
	// SystemdCgroup enable systemd cgroup support
	SystemdCgroup() bool
	// Rootless enable rootless mode
	// nil means auto detection
	Rootless() *bool
}

// Runnable defines information that are used for
// Create, Run and Restore commands
type Runnable interface {
	Base
	// ID returns the container's id
	ID() string
	// NoPivot do not use pivot root to jail process inside rootfs
	NoPivot() bool
	// NoNewKeyring do not create a keyring for the container
	NoNewKeyring() bool
	NotifySocket() string
}

// BaseBuilder is used for creating the Base context
type BaseBuilder interface {
	SetRoot(string)
	SetCriu(string)
	SetSystemdCgroup(bool)
	SetRootless(*bool)
	getResult() Base
}

// RunnableBuilder is used for creating the Runnable context
type RunnableBuilder interface {
	BaseBuilder
	SetID(string)
	SetNoPivot(string)
	SetNoNewKeyring(bool)
	SetNotifySocket() string
}
