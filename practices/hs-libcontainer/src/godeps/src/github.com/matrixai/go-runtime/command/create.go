package command

import (
	"github.com/matrixai/go-runtime/context"
)

// CreateCommand handles the creation of containers
type CreateCommand struct{}

// Execute sets up the environment for the container.
func (cmd *CreateCommand) Execute(ctx *context.Context) (interface{}, error) {
	spec, err := setupSpec(ctx)
	if err != nil {
		return nil, err
	}
	return nil, nil
}
