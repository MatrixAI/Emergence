package command

import (
	"fmt"
	"github.com/opencontainers/runc/libcontainer"
	"github.com/opencontainers/runc/libcontainer/system"
	"github.com/opencontainers/runtime-spec/specs-go"
	"strconv"
	"strings"
)

const (
	exactArgs = iota
	minArgs
	maxArgs
)

// checkArgs checks the argument size
func checkArgs(ctx *Context, expected int, typeCheck int) error {
	switch typeCheck {
	case exactArgs:
		if len(ctx.Args) != expected {
			return fmt.Errorf("This command requires exactly %d argument(s)", expected)
		}
	case minArgs:
		if len(ctx.Args) < expected {
			return fmt.Errorf("This command requires at least %d argument(s)", expected)
		}
	case maxArgs:
		if len(ctx.Args) > expected {
			return fmt.Errorf("This command can have at most %d argument(s)", expected)
		}
	}
	return nil
}

// parseBoolOrAuto returns (nil, nil) if s is empty or "auto"
func parseBoolOrAuto(s string) (*bool, error) {
	if s == "" || strings.ToLower(s) == "auto" {
		return nil, nil
	}
	b, err := strconv.ParseBool(s)
	return &b, err
}

func isRootless(ctx *Context) (bool, error) {
	if ctx != nil {
		if val, ok := ctx.Options["rootless"]; ok {
			b, err := parseBoolOrAuto(val)
			if err != nil {
				return false, err
			}
			if b != nil {
				return *b, nil
			}
		}
	}
	return system.GetParentNSeuid() != 0 || system.RunningInUserNS(), nil
}

func createContainer(ctx *Context, id string, spec *specs.Spec) (libcontainer.Container, error) {
	rootless, err := isRootless(ctx)
	if err != nil {
		return nil, err
	}
}
