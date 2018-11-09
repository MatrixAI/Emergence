package main

// #include <data.h>
import "C"

//export foo
func foo(in *C.struct_InputStruct, out *C.struct_ReturnStruct) {
	// Do some operations...
	out.val = in.val
}

func main() {}
