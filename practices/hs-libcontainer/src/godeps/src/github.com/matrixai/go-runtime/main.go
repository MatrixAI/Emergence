// Defines the cgo API for container runtime
package main

// #include <command.h>
import "C"

var configName = "config.json"

func cBool(n C.bool) bool {
	if n == 0 {
		return false
	}
	return true
}

func unmarshalBaseCommand(c *C.struct_BaseCommand) *BaseCommand {
	var rootless *bool
	if c.rootless == nil {
		rootless = nil
	} else {
		*rootless = cBool(*c.rootless)
	}

	return &BaseCommand{
		statePath:     C.GoString(c.statePath),
		criu:          C.GoString(c.criu),
		systemdCgroup: cBool(c.systemdCgroup),
		rootless:      rootless,
	}
}

func unmarshalRunnableCommand(c *C.struct_RunnableCommand) *RunnableCommand {
	return &RunnableCommand{
		BaseCommand:  *unmarshalBaseCommand(c.base),
		id:           C.GoString(c.id),
		noPivot:      cBool(c.noPivot),
		noNewKeyring: cBool(c.noPivot),
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

//export create
func create(cc *C.struct_CreateCommand) (int, error) {
	cmd := unmarshalCreateCommand(cc)
	s, err := cmd.Execute()
	if status, ok := s.(int); ok {
		return status, err
	}
	return -1, err
}

func main() {}
