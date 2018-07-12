package main

import (
	"errors"
	"fmt"
	"github.com/opencontainers/runc/libcontainer"
	"github.com/opencontainers/runc/libcontainer/cgroups/systemd"
	"github.com/opencontainers/runc/libcontainer/intelrdt"
	"github.com/opencontainers/runc/libcontainer/specconv"
	"github.com/opencontainers/runc/libcontainer/system"
	"github.com/opencontainers/runtime-spec/specs-go"
	"os/exec"
	"path/filepath"
)

var errEmptyID = errors.New("container id cannot be empty")

func isRootless(ctx *Context) (bool, error) {
	if (*ctx).Rootless() != nil {
		return *(*ctx).Rootless(), nil
	}
	return system.GetParentNSeuid() != 0 || system.RunningInUserNS(), nil
}

// loadFactory returns the configured factory instance for execing containers
func loadFactory(ctx *Context) (libcontainer.Factory, error) {
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
	// to avoid doing a path lookup in the nsexec  TODO: The binary
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

type CtAct uint8

const (
	CT_ACT_CREATE CtAct = iota
)

// createContainer returns a container instance
func createContainer(ctx *Context, spec *specs.Spec) (libcontainer.Container, error) {
	createCtx, ok := (*ctx).(*CtxCreate)
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
	if err != nil {
		return nil, err
	}
	factory, err := loadFactory(ctx)
	if err != nil {
		return nil, err
	}
	return factory.Create(createCtx.ContainerID, config)
}

func startContainer(ctx *Context, spec *specs.Spec, action CtAct, criuOpts *libcontainer.CriuOpts) (int, error) {
	var id string
	switch action {
	case CT_ACT_CREATE:
		createCtx, ok := (*ctx).(*CtxCreate)
		id = createCtx.ContainerID
	}

	if id == "" {
		return -1, errEmptyID
	}

	notifySocker := newNotifySocket(ctx.NotifySocket(), id)
	if notifySocket != nil {
		notifySocket.setupSpec(context, spec)
	}

	container, err := createContainer(ctx, spec)
}
