package main

import (
"fmt"
)

type T interface {
	a() string
}

type A struct {}

func (*A) a() string {return "hello"}

type B struct {
	A
}

func (*B) b() string {return "world"}

type InterfaceFoo interface {
	foo(s *T)
}

type Create struct {}
func (*Create) foo(s *T) {
	fmt.Println("foo is called")
	fmt.Println(B(s).b())
}

func VIP(f *InterfaceFoo) {
	fmt.Println("create is still an interface!")
}

func main() {
	b := &B{A{}}
	fmt.Println(b.a())
	create := &Create{}
	create.foo(b)
	VIP(create)
}
