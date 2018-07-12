package main

import (
	"fmt"
	"github.com/opencontainers/runtime-spec/specs-go"
	"os"
)

const (
	exactArgs = iota
	minArgs
	maxArgs
)

// setupSpec performs initial setup based on given context
func setupSpec(ctx *Context) (*specs.Spec, error) {
	createCtx := (*ctx).(*CtxCreate)
	bundle := createCtx.Bundle
	if bundle == nil {
		return nil, fmt.Errorf("Bundle is not given")
	}
	if *bundle != "" {
		if err := os.Chdir(*bundle); err != nil {
			return nil, err
		}
	}
	spec, err := loadSpec(configName)
	if err != nil {
		return nil, err
	}
	return spec, nil
}
