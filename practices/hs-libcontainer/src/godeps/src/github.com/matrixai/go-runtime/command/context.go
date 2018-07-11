// Package context carries the information passed into each Command
package command

// Context contains infomation being passed
type Context struct {
	Args    map[string]string
	Options map[string]string
}

// New returns a default Context for all Commands
func New() (ctx *Context) {
	ctx = &Context{
		map[string]string{},
		map[string]string{ // Adding shared options
			"root":     "/run/runc",
			"criu":     "criu",
			"rootless": "auto",
			"systemd-cgroup": "false"
		},
	}
	return
}
