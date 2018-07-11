// Defines the cgo API for container runtime
package main

import (
	"github.com/matrixai/go-runtime/command"
)

var configName = "config.json"

func main() {
	ctx := command.Context{
		[]string{"cont01"},
		map[string]string{
			"bundle": "tests/mycontainer",
			"config": "config.json",
		},
	}
	cmd, _ := command.Factory(command.CREATE)
	cmd.Execute(&ctx)
}
