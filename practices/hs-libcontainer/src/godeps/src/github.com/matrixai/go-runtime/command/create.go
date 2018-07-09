package command

var configName = "config.json"

type CreateCommand struct {
	containerId string
	bundlePath  string
}

func (cmd *CreateCommand) Load(ctx *Context) error {
	if err := checkArgs(ctx, 1, exactArgs); err != nil {
		return err
	}
	cmd.containerId = ctx.Args[0]
	cmd.bundlePath = ctx.Options["bundle"]
	return nil
}

func (cmd *CreateCommand) Execute(ctx *Context) (interface{}, error) {
	spec, err := setupSpec(cmd.bundlePath)
	if err != nil {
		return nil, err
	}
}
