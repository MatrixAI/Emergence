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

func newNotifySocket(statePath string, host string, id string) *notifySocket {
	if host == "" {
		return nil
	}
	root := filepath.Join(statePath, id)
	path := filepath.Join(root, "notify.sock")

	notifySocket := &notifySocket{
		socket:     nil,
		host:       host,
		socketPath: path,
	}

	return notifySocket
}

func (ns *notifySocket) Close() error {
	return ns.socket.Close()
}

// If systemd is supporting sd_notify protocol, this function will add support
// for sd_notify protocol from within the container.
func (ns *notifySocket) setupSpec(spec *specs.Spec) {
	mount := specs.Mount{Destination: ns.host, Source: ns.socketPath, Options: []string{"bind"}}
	spec.Mounts = append(spec.Mounts, mount)
	spec.Process.Env = append(spec.Process.Env, fmt.Sprintf("NOTIFY_SOCKET=%s", ns.host))
}

func (ns *notifySocket) setupSocket() error {
	addr := net.UnixAddr{
		Name: ns.socketPath,
		Net:  "unixgram",
	}
	socket, err := net.ListenUnixgram("unixgram", &addr)
	if err != nil {
		return err
	}
	ns.socket = socket
	return nil
}

// pid1 must be set only with -d, as it is used to set the new process as the main process
// for the service in systemd
func (ns *notifySocket) run(pid1 int) {
	buf := make([]byte, 512)
	notifySocketHostAddr := net.UnixAddr{Name: ns.host, Net: "unixgram"}
	client, err := net.DialUnix("unixgram", nil, &notifySocketHostAddr)
	if err != nil {
		return
	}
	for {
		r, err := ns.socket.Read(buf)
		if err != nil {
			break
		}
		var out bytes.Buffer
		for _, line := range bytes.Split(buf[0:r], []byte{'\n'}) {
			if bytes.HasPrefix(line, []byte("READY=")) {
				_, err = out.Write(line)
				if err != nil {
					return
				}
				_, err = out.Write([]byte{'\n'})
				if err != nil {
					return
				}
				_, err = client.Write(out.Bytes())
				if err != nil {
					return
				}
				// now we can inform systemd to use pid1 as the pid to monitor
				if pid1 > 0 {
					newPid := fmt.Sprintf("MAINPID=%d\n", pid1)
					client.Write([]byte(newPid))
				}
				return
			}
		}
	}
}
