package context



func (s *Create) Root() string        { return s.root }
func (s *Create) Criu() string        { return s.criu }
func (s *Create) SystemdCgroup() bool { return s.systemdCgroup }
func (s *Create) Rootless() *bool     { return s.rootless }

func (s *Create) ID() string           { return s.id }
func (s *Create) NoPivot() bool        { return s.noPivot }
func (s *Create) NoNewKeyring() bool   { return s.noNewKeyring }
func (s *Create) NotifySocket() string { return s.notifySocket }

type CreateBuilder struct {
	create *Create
}

func NewCreateBuilder() *CreateBuilder {
	return &CreateBuilder{
		&Create{
			root:          "/run/user/1002/runc",
			criu:          "criu",
			systemdCgroup: false,
			rootless:      nil,
			id:            id,
			noPivot:       false,
			noNewKeyring:  false,
			notifySocket:  "",
			listenFds:     -1,
			Bundle:        bundle,
			ConsoleSocket: nil,
			PidFile:       "",
			PreserveFds:   0,
		},
	}
}

CreateBuilder Set
