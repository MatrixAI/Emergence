// Defines the cgo API for container runtime
package main

// #include <command.h>
import "C"

var configName = "config.json"

//export create
func create(cc *C.struct_CreateCommand) (int, error) {
	cmd := unmarshalCreateCommand(cc)
	s, err := cmd.Execute()
	if status, ok := s.(int); ok {
		return status, err
	}
	return -1, err
}

func goBool(c C.bool) bool {
	if c {
		return true
	}
	return false
}

func unmarshalBaseCommand(c *C.struct_BaseCommand) *BaseCommand {
	var rootless *bool
	switch c.rootless {
	case 0: // auto
		rootless = nil
	case 1: // rootless
		b := true
		rootless = &b
	case 2: // not rootless
		b := false
		rootless = &b
	}

	return &BaseCommand{
		statePath:     C.GoString(c.statePath),
		criu:          C.GoString(c.criu),
		systemdCgroup: goBool(c.systemdCgroup),
		rootless:      rootless,
	}
}

func unmarshalRunnableCommand(c *C.struct_RunnableCommand) *RunnableCommand {
	return &RunnableCommand{
		BaseCommand:  *unmarshalBaseCommand(c.base),
		id:           C.GoString(c.id),
		noPivot:      goBool(c.noPivot),
		noNewKeyring: goBool(c.noPivot),
		listenFds:    int(c.listenFds),
	}
}

func unmarshalCreateCommand(c *C.struct_CreateCommand) *CreateCommand {
	return &CreateCommand{
		RunnableCommand: *unmarshalRunnableCommand(c.runnable),
		bundle:          C.GoString(c.bundle),
		consoleSocket:   C.GoString(c.consoleSocket),
		pidFile:         C.GoString(c.pidFile),
		preserveFds:     int(c.preserveFds),
	}
}

func main() {}
