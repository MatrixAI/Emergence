package main

// Context defines necessary information that can be used for any command
type Context interface {
	// Root directory for storage of container state (should be in tmpfs)
	Root() string
	// Criu path to the briu binary
	Criu() string
	// SystemdCgroup enable systemd cgroup support
	SystemdCgroup() bool
	// Rootless enable rootless mode
	// nil means auto
	Rootless() *bool
}

type Runnable interface {
	NotifySocket() string
	ContainerID() string
	NoPivot() bool
}

// BaseContext defines the default attributes for the Context interface
type BaseContext struct {
	root          string
	criu          string
	systemdcgroup bool
	rootless      *bool
}

// Root returns default value for container states
func (b *BaseContext) Root() string { return "/run/user/1002/runc" }

// Criu returns default value for criu binary
func (b *BaseContext) Criu() string { return "criu" }

// SystemdCgroup returns default value for systemdcgroup
func (b *BaseContext) SystemdCgroup() bool { return false }

// Rootless returns default value for rootless container
func (b *BaseContext) Rootless() *bool { return nil }

// NewContext is a factory method used for ceating Contextes for Commands
func NewContext(cType Type) (Context, error) {
	switch cType {
	case CREATE:
		return &CtxCreate{
			Bundle:        nil,
			ConsoleSocket: nil,
			PidFile:       nil,
			NoPivot:       false,
			NoNewKeyring:  false,
			PreserveFds:   0,
		}, nil
	default:
		return nil, nil // Unknown cType: should not happen
	}
}
