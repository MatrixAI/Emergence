package main

// #include <data.h>
import "C"
import (
	"github.com/matrixai/test/data"
)


func copyVal(s *data.InputStruct) *data.ReturnStruct {
	return &data.ReturnStruct{ s.Val }
}

//export foo
func foo(in *C.struct_InputStruct, out *C.struct_ReturnStruct) {
	// Do some operations...
	out.val = in.val
}

func main() {}
