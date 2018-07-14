package main

import (
"fmt"
)

type Animal struct {
	name string
}

type Cat struct {
	Animal
	color string
}

func main() {
	a := Cat{Animal{"George"}, "ginger"}
	fmt.Println(a.name)
}

