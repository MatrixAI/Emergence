package main

import (
	"fmt"
	"github.com/opencontainers/runc/libcontainer"
	"github.com/opencontainers/runtime-spec/specs-go"
	"golang.org/x/sys/unix"
	"os"
	"strconv"
)

type CtAct uint8

const (
	CT_ACT_CREATE = iota + 1
	CT_ACT_RUN
	CT_ACT_RESTORE
)

type runner struct {
	init            bool
	enableSubreaper bool
	shouldDestroy   bool
	detach          bool
	listenFDs       []*os.File
	preserveFDs     int
	pidFile         string
	consoleSocket   string
	container       libcontainer.Container
	action          CtAct
	notifySocket    *notifySocket
	criuOpts        *libcontainer.CriuOpts
}

func (r *runner) run(config *specs.Process) (int, error) {
	if err := r.checkTerminal(config); err != nil {
		r.destroy()
		return -1, err
	}
	process, err := newProcess(*config, r.init)
	if err != nil {
		r.destroy()
		return -1, err
	}
	if len(r.listenFDs) > 0 {
		process.Env = append(process.Env, fmt.Sprintf("LISTEN_FDS=%d", len(r.listenFDs)), "LISTEN_PID=1")
		process.ExtraFiles = append(process.ExtraFiles, r.listenFDs...)
	}
	baseFd := 3 + len(process.ExtraFiles)
	for i := baseFd; i < baseFd+r.preserveFDs; i++ {
		process.ExtraFiles = append(process.ExtraFiles, os.NewFile(uintptr(i), "PreserveFD:"+strconv.Itoa(i)))
	}
	rootuid, err := r.container.Config().HostRootUID()
	if err != nil {
		r.destroy()
		return -1, err
	}
	rootgid, err := r.container.Config().HostRootGID()
	if err != nil {
		r.destroy()
		return -1, err
	}
	var (
		detach = r.detach || (r.action == CT_ACT_CREATE)
	)
	// Setting up IO is a two stage process. We need to modify process to deal
	// with detaching containers, and then we get a tty after the container has
	// started.
	handler := newSignalHandler(r.enableSubreaper, r.notifySocket)
	tty, err := setupIO(process, rootuid, rootgid, config.Terminal, detach, r.consoleSocket)
	if err != nil {
		r.destroy()
		return -1, err
	}
	defer tty.Close()
	switch r.action {
	case CT_ACT_CREATE:
		err = r.container.Start(process)
	case CT_ACT_RESTORE:
		err = r.container.Restore(process, r.criuOpts)
	case CT_ACT_RUN:
		err = r.container.Run(process)
	default:
		panic("Unknown action")
	}
	if err != nil {
		r.destroy()
		return -1, err
	}
	if err = tty.waitConsole(); err != nil {
		r.terminate(process)
		r.destroy()
		return -1, err
	}
	if err = tty.ClosePostStart(); err != nil {
		r.terminate(process)
		r.destroy()
		return -1, err
	}
	if r.pidFile != "" {
		if err = createPidFile(r.pidFile, process); err != nil {
			r.terminate(process)
			r.destroy()
			return -1, err
		}
	}
	status, err := handler.forward(process, tty, detach)
	if err != nil {
		r.terminate(process)
	}
	if detach {
		return 0, nil
	}
	r.destroy()
	return status, err
}
func (r *runner) destroy() {
	if r.shouldDestroy {
		destroy(r.container)
	}
}
func (r *runner) terminate(p *libcontainer.Process) {
	_ = p.Signal(unix.SIGKILL)
	_, _ = p.Wait()
}

func (r *runner) checkTerminal(config *specs.Process) error {
	detach := r.detach || (r.action == CT_ACT_CREATE)
	// Check command-line for sanity.
	if detach && config.Terminal && r.consoleSocket == "" {
		return fmt.Errorf("cannot allocate tty if runc will detach without setting console socket")
	}
	if (!detach || !config.Terminal) && r.consoleSocket != "" {
		return fmt.Errorf("cannot use console socket if runc will not detach or allocate tty")
	}
	return nil
}
