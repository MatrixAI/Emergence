// This file doesn't get used by the FFI practice
// But it shows the marhsalling of C structs to Go structs and vice versa.

package main

// #include <data.h>
import "C"

import (
	"github.com/matrixai/test/data"
)

func ConvInputStruct(f *C.struct_InputStruct) *data.InputStruct {
	return &data.InputStruct { int(f.val) }
}


func ConvReturnStruct(b *data.ReturnStruct) *C.struct_ReturnStruct {
	return &C.struct_ReturnStruct { C.int(b.Val) }
}

