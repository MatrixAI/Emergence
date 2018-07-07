package main

// #include <data.h>
import "C"
import (
	"github.com/matrixai/test/data"
)


func bar(f *data.Fooz) *data.Barz {
	return &data.Barz{ f.N }
}

//export foo
func foo(f *C.struct_Fooz) (*C.struct_Barz, error) {
	// Do some operations...
	return ConvBarz(bar(ConvFooz(f))), nil
}

func main() {}
