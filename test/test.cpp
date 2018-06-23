#include <iostream>
#include "Foo_stub.h"

struct MyStruct {
    int foo;
    char bar;
};

int main(int argc, char *argv[])
{
    MyStruct myStruct;

    myStruct.foo = 7;
    myStruct.bar = 'x';

    hs_init(&argc, &argv);
    std::cout << foo(&myStruct) << "\n";
    showStruct(&myStruct);
    hs_exit();
    return 0;
}

