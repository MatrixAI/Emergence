// Package context carries the information passed into each Command
package context

// Context defines necessary information that can be used for any command
type Context interface {
	// Config file filename
	Config() string
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

// BaseContext defines the default attributes for the Context interface
type BaseContext struct{}

// Config returns defalt value for config filenames
func (*BaseContext) Config() string { return "config.json" }

// Root returns default value for container states
func (*BaseContext) Root() string { return "/run/user/1002/runc" }

// Criu returns default value for criu binary
func (*BaseContext) Criu() string { return "criu" }

// SystemdCgroup returns default value for systemdcgroup
func (*BaseContext) SystemdCgroup() bool { return false }

// Rootless returns default value for rootless container
func (*BaseContext) Rootless() *bool { return nil }

// Type is an enum representing command type
type Type int

const (
	// CREATE Creates the container environment
	CREATE Type = iota
	// START Start the processes within a container
	START
	// KILL Send a signal to a running container
	KILL
	// DELETE Deletes a container
	DELETE
	// STATE the state of the container
	STATE
)

// New is a factory method used for ceating Contextes for Commands
func New(cType Type) (Context, error) {
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
