// Defines the cgo API for container runtime
package main

// #include <command.h>
import "C"

import (
	"github.com/davecgh/go-spew/spew"
)

var configName = "config.json"

//export create
func create(cc *C.struct_CreateCommand) int {
	cmd := unmarshalCreateCommand(cc)
	s, err := cmd.Execute()
	spew.Dump(err)
	if status, ok := s.(int); ok {
		return status
	}
	return -1
}

func goBool(c C.bool) bool {
	if c {
		return true
	}
	return false
}

func unmarshalBaseCommand(c *C.struct_BaseCommand) *baseCommand {
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

	return &baseCommand{
		statePath:     C.GoString(c.statePath),
		criu:          C.GoString(c.criu),
		systemdCgroup: goBool(c.systemdCgroup),
		rootless:      rootless,
	}
}

func unmarshalRunnableCommand(c *C.struct_RunnableCommand) *runnableCommand {
	return &runnableCommand{
		baseCommand:  *unmarshalBaseCommand(c.base),
		id:           C.GoString(c.id),
		noPivot:      goBool(c.noPivot),
		noNewKeyring: goBool(c.noPivot),
	}
}

func unmarshalCreateCommand(c *C.struct_CreateCommand) *createCommand {
	return &createCommand{
		runnableCommand: *unmarshalRunnableCommand(c.runnable),
		bundle:          C.GoString(c.bundle),
		consoleSocket:   C.GoString(c.consoleSocket),
		pidFile:         C.GoString(c.pidFile),
		preserveFds:     int(c.preserveFds),
	}
}

func main() {}
