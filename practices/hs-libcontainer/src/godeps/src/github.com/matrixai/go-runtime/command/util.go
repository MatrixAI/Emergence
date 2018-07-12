package command

import (
	"fmt"
	"github.com/matrixai/go-runtime/context"
	"github.com/opencontainers/runc/libcontainer"
	"github.com/opencontainers/runc/libcontainer/cgroups/systemd"
	"github.com/opencontainers/runc/libcontainer/intelrdt"
	"github.com/opencontainers/runc/libcontainer/specconv"
	"github.com/opencontainers/runc/libcontainer/system"
	"github.com/opencontainers/runtime-spec/specs-go"
	"os"
	"os/exec"
	"path/filepath"
)

const (
	exactArgs = iota
	minArgs
	maxArgs
)

// setupSpec performs initial setup based on given context
func setupSpec(ctx *context.Context) (*specs.Spec, error) {
	createCtx := (*ctx).(*context.CtxCreate)
	bundle := createCtx.Bundle
	if bundle == nil {
		return nil, fmt.Errorf("Bundle is not given")
	}
	if *bundle != "" {
		if err := os.Chdir(*bundle); err != nil {
			return nil, err
		}
	}
	spec, err := loadSpec((*ctx).Config())
	if err != nil {
		return nil, err
	}
	return spec, nil
}

func isRootless(ctx *context.Context) (bool, error) {
	if (*ctx).Rootless() != nil {
		return *(*ctx).Rootless(), nil
	}
	return system.GetParentNSeuid() != 0 || system.RunningInUserNS(), nil
}

// loadFactory returns the configured factory instance for execing containers
func loadFactory(ctx *context.Context) (libcontainer.Factory, error) {
	root := (*ctx).Root()
	rootAbs, err := filepath.Abs(root)
	if err != nil {
		return nil, err
	}

	// We default to cgroupfs, and can only use systemd if the system
	// is a systemd box.
	cgroupManager := libcontainer.Cgroupfs
	rootless, err := isRootless(ctx)
	if err != nil {
		return nil, err
	}
	if rootless {
		cgroupManager = libcontainer.RootlessCgroupfs
	}
	if (*ctx).SystemdCgroup() {
		if systemd.UseSystemd() {
			cgroupManager = libcontainer.SystemdCgroups
		} else {
			return nil, fmt.Errorf("systemd cgroup flag passed, but systemd support for managing cgroup is not available")
		}
	}

	intelRdtManager := libcontainer.IntelRdtFs
	if !intelrdt.IsEnabled() {
		intelRdtManager = nil
	}

	// We resolve the paths for {newuidmap,newgidmap} from the context of runc,
	// to avoid doing a path lookup in the nsexec context. TODO: The binary
	// names are not currently configurable.

	newuidmap, err := exec.LookPath("newuidmap")
	if err != nil {
		newuidmap = ""
	}
	newgidmap, err := exec.LookPath("newgidmap")
	if err != nil {
		newgidmap = ""
	}

	return libcontainer.New(rootAbs, cgroupManager, intelRdtManager,
		libcontainer.CriuPath((*ctx).Criu()),
		libcontainer.NewuidmapPath(newuidmap),
		libcontainer.NewgidmapPath(newgidmap))
}

func createContainer(ctx *context.Context, spec *specs.Spec) (libcontainer.Container, error) {
	createCtx, ok := (*ctx).(*context.CtxCreate)
	if !ok {
		return nil, fmt.Errorf("casting to ContextCreate failed")
	}
	rootless, err := isRootless(ctx)
	if err != nil {
		return nil, err
	}
	config, err := specconv.CreateLibcontainerConfig(
		&specconv.CreateOpts{
			CgroupName:       *createCtx.ContainerID,
			UseSystemdCgroup: createCtx.SystemdCgroup(),
			NoPivotRoot:      createCtx.NoPivot,
			NoNewKeyring:     createCtx.NoNewKeyring,
			Spec:             spec,
			Rootless:         rootless,
		})
	if err != nil {
		return nil, err
	}
	factory, err := loadFactory(ctx)
	if err != nil {
		return nil, err
	}
	return factory.Create(*createCtx.ContainerID, config)
}
