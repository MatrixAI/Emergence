// util.go contains helper functions for the container runtime
package command

import (
	"fmt"
	"github.com/matrixai/github.com/matrixai/go-artifact-runtime/context"
)

const (
	exactArgs = iota
	minArgs
	maxArgs
)

func checkArgs(ctx *Context, expected int, typeCheck int) error {
	var err error = nil
	switch typeCheck {
	case exactArgs:
		if len(ctx.Args) != expected {
			err := fmt.Errorf("This command requires exactly %d argument(s)", expected)
		}
	case minArgs:
		if len(ctx.Args) < expected {
			err := fmt.Errorf("This command requires at least %d argument(s)", expected)
		}
	case maxArgs:
		if len(ctx.Args) > expected {
			err := fmt.Errorf("This command can have at most %d argument(s)", expected)
		}
	}
	return err
}
