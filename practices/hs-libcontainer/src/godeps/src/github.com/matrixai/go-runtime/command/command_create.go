package command

import (
	"fmt"
)

type createCommand struct{}

func (cmd *createCommand) Execute(ctx *Context) (interface{}, error) {
	createCtx, ok := (*ctx).(*ContextCreate)
	if err := checkArgs(ctx, 1, exactArgs); err != nil {
		return nil, err
	}
	bundle := createCtx.Bundle
	if bundle == nil {
		return nil, fmt.Errorf("missing bundle path")
	}
	config := createCtx.Config()
	if config == "" {
		return nil, fmt.Errorf("config filename is empty")
	}
	spec, err := setupSpec(*bundle, config)
	if err != nil {
		return nil, err
	}
	return nil, nil
}
