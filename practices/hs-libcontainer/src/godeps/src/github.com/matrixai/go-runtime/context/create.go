package context

// CtxCreate holds the information needed for the "create" command
type CtxCreate struct {
	BaseContext
	// ContainerID container identifier, must be specified
	// nil for unspecified
	ContainerID *string
	// Bundle path to the bundle, must be specified
	// nil for unspecified
	Bundle *string
	// ConsoleSocket path to an AF_UNIX socket which will receive a fd
	// referencing the master end of the console's pseudoterminal
	// nil for unspecified
	ConsoleSocket *string
	// PidFile a file to write the process id to
	// nil for unspecified
	PidFile *string
	// NoPivot do not use pivot root to jail process inside rootfs
	NoPivot bool
	// NoNewKeyring do not create a keyring for the container
	NoNewKeyring bool
	// PreserveFds Pass N additional fds to the container
	// (stdio + $LISTEN_FDS + N) in total
	PreserveFds int
}
