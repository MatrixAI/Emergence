
package main

import "C"

import "fmt"


//export Testing
func Testing(a int) int {
	return a + 2
}

func strFxn(input string) string {
	return "Hello " + input + " World"
}

//export StrFxn
func StrFxn(cinput *C.char) *C.char {
	input := C.GoString(cinput)
	fmt.Println("Hello")
	return C.CString(strFxn(input))
}

func main() {}
