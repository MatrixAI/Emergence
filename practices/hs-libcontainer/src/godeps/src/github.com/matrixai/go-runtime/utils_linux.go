package main

import (
	"errors"
	"github.com/opencontainers/runc/libcontainer/system"
)

var errEmptyID = errors.New("container id cannot be empty")

func isRootless(rootless *bool) (bool, error) {
	if rootless != nil {
		return *rootless, nil
	}
	return system.GetParentNSeuid() != 0 || system.RunningInUserNS(), nil
}
