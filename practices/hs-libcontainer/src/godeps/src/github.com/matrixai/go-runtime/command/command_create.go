package command

type createContext struct {
	containerID string
	bundlePath  string
	configName  string
}

type createCommand struct{}

func (cmd *createCommand) Execute(ctx *Context) (interface{}, error) {
	if err := checkArgs(ctx, 1, exactArgs); err != nil {
		return nil, err
	}

	var cc = &createContext{
		ctx.Args["container-id"],
		ctx.Options["bundle"],
		ctx.Options["config"],
	}

	spec, err := setupSpec(cc.bundlePath, cc.configName)
	if err != nil {
		return nil, err
	}

	return nil, nil
}
