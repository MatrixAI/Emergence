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

func (*BaseContext) Config() string      { return "config.json" }
func (*BaseContext) Root() string        { return "/run/user/1002/runc" }
func (*BaseContext) Criu() string        { return "criu" }
func (*BaseContext) SystemdCgroup() bool { return false }
func (*BaseContext) Rootless() *bool     { return nil }

// New is a factory method used for ceating Contextes for Commands
func Factory(cType Type) (Context, error) {
	switch cType {
	case CREATE:
		return &ContextCreate{
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
