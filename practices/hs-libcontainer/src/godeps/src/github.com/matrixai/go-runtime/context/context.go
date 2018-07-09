package context

// Context carries the information passed into each API call
type Context struct {
	Type    CommandType
	Args    []string
	Options map[string]string
}

// CommandType is an enum representing command type
type CommandType int

const (
	// Creates the container environment
	CREATE CommandType = iota
	// Start the processes within a container
	START
	// Send a signal to a running container
	KILL
	// Deletes a container
	DELETE
	// Return the state of the container
	STATE
)
