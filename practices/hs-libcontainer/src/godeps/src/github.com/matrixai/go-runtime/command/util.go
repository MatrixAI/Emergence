package command

import (
	"fmt"
	"github.com/opencontainers/runc/libcontainer"
	"github.com/opencontainers/runc/libcontainer/specconv"
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

// parseBoolOrAuto returns (nil, nil) if s is empty or "auto"
func parseBoolOrAuto(s string) (*bool, error) {
	if s == "" || strings.ToLower(s) == "auto" {
		return nil, nil
	}
	b, err := strconv.ParseBool(s)
	return &b, err
}

func isRootless(ctx *Context) (bool, error) {
	if (*ctx).Rootless() != nil {
		return *(*ctx).Rootless(), nil
	}
	return system.GetParentNSeuid() != 0 || system.RunningInUserNS(), nil
}

func createContainer(ctx *Context, spec *specs.Spec) (libcontainer.Container, error) {
	createCtx, ok := (*ctx).(*ContextCreate)
	if !ok {
		return nil, fmt.Errorf("casting to ContextCreate failed")
	}
	rootless, err := isRootless(ctx)
	if err != nil {
		return nil, err
	}
	config, err := specconv.CreateLibcontainerConfig(
		&specconv.CreateOpts{
			CgroupName:       createCtx.ContainerID,
			UseSystemdCgroup: createCtx.SystemdCgroup(),
			NoPivotRoot:      createCtx.NoPivot,
			NoNewKeyring:     createCtx.NoNewKeyring,
			Spec:             spec,
			Rootless:         rootless,
		})

}
