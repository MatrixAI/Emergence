package main

import (
	"fmt"
	"github.com/opencontainers/runc/libcontainer"
	"github.com/opencontainers/runc/libcontainer/cgroups/systemd"
	"github.com/opencontainers/runc/libcontainer/intelrdt"
	"os/exec"
	"path/filepath"
)

type command interface {
	Execute() (interface{}, error)
}

type baseCommand struct {
	statePath     string
	criu          string
	systemdCgroup bool
	rootless      *bool
}

type runnableCommand struct {
	baseCommand
	id           string
	noPivot      bool
	noNewKeyring bool
	notifySocket string
	listenFds    int
}

func (cmd *baseCommand) loadFactory() (libcontainer.Factory, error) {
	statePathAbs, err := filepath.Abs(cmd.statePath)
	if err != nil {
		return nil, err
	}

	// We default to cgroupfs, and can only use systemd if the system
	// is a systemd box
	cgroupManager := libcontainer.Cgroupfs
	rootless, err := isRootless(cmd.rootless)
	if err != nil {
		return nil, err
	}
	if rootless {
		cgroupManager = libcontainer.RootlessCgroupfs
	}
	if cmd.systemdCgroup {
		if systemd.UseSystemd() {
			cgroupManager = libcontainer.SystemdCgroups
		} else {
			return nil, fmt.Errorf("systemd sypport for managing cgroup is not available")
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

	return libcontainer.New(statePathAbs, cgroupManager, intelRdtManager,
		libcontainer.CriuPath(cmd.criu),
		libcontainer.NewuidmapPath(newuidmap),
		libcontainer.NewgidmapPath(newgidmap))

}
