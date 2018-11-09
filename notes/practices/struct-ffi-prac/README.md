# struct-ffi-prac

This is a FFI practice for calling a function foo(struct1, struct2) from Haskell to Go, where struct1 contains some inputs and struct2 contains the necessary outputs for the Haskell code.


To build the files, run:

```bash
# Ensure that the environment has stack and go

mkdir -p $GOPATH/src/MatrixAI/struct-ffi-prac/
cd $GOPATH/src/MatrixAI/struct-ffi-prac

# Copy the files to the current location.

cd src/godep/
make
ln -sf libtest.so ../../
cd ../../

stack build

# This is necessary for the executable to pick up the .so file
export LD_LIBRARY_PATH="$(pwd):$LDLIBRARY_PATH"

stack exec struct-ffi-prac-exe
```

