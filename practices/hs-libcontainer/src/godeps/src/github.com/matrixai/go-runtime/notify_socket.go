package main

import (
	"bytes"
	"fmt"
	"github.com/opencontainers/runtime-spec/specs-go"
	"net"
	"path/filepath"
)

type notifySocket struct {
	socket     *net.UnixConn
	host       string
	socketPath string
}

func newNotifySocket(ctx *Context, host string, id string) *notifySocket {
	if host == "" {
		return nil
	}
	root := filepath.Join((*ctx).Root(), id)
	path := filepath.Join(root, "notify.sock")

	notifySocket := &notifySocket{
		socket:     nil,
		host:       host,
		socketPath: path,
	}

	return notifySocket
}

// If systemd is supporting sd_notify protocol, this function will add support
// for sd_notify protocol from within the container.
func (s *notifySocket) setupSpec(spec *specs.Spec) {
	mount := specs.Mount{Destination: s.host, Source: s.socketPath, Options: []string{"bind"}}
	spec.Mounts = append(spec.Mounts, mount)
	spec.Process.Env = append(spec.Process.Env, fmt.Sprintf("NOTIFY_SOCKET=%s", s.host))
}

func (s *notifySocket) setupSocket() error {
	addr := net.UnixAddr{
		Name: s.socketPath,
		Net:  "unixgram",
	}
	socket, err := net.ListenUnixgram("unixgram", &addr)
	if err != nil {
		return err
	}
	s.socket = socket
	return nil
}
