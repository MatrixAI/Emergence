package main

// #include <data.h>
import "C"

import (
	"github.com/matrixai/test/data"
)

func ConvFooz(f *C.struct_Fooz) *data.Fooz {
	return &data.Fooz { int(f.n) }
}


func ConvBarz(b *data.Barz) *C.struct_Barz {
	return nil // TODO
}

