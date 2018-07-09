package command

import (
	"fmt"
)

// Command interface specifies the necessary operations
// needed for each API call.
type Command interface {
	Execute(*Context) (interface{}, error)
}

type CommandFactory interface {
	// Create a Command instance
	Create(CommandType) (*Command, error)
}

func (*CommandFactory) Create(ct CommandType) (cmd *Command, err error) {
	switch cmdType {
	case CREATE:
		cmd := &CreateCommand{}
	// TODO: more commands to add
	default:
		err := fmt.Errorf("unknown command type")
	}
	return cmd, err
}
