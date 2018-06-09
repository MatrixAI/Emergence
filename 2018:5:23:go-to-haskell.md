# Setting up a Go project
We will set up a Go development environment using `Go tool`.

- All Go code are kept in a single *workspace*
- A work space contains many version control *repos*
- Each repo contain one or more *packages*
- Each package consists of one or more Go source code in a single directory
- The path to the package's directory determines its *import path*

## Workspace
- Has three directories at root: `src`, `pkg`, and `bin`
- The go tool builds source packages and installs the resulting binaries to the `pkg` and `bin` directories.
- The `src` subdirectory typically contains multiple version control repositories that tracks developments of one or more source packages.

## GOPATH environment variable
- Specifies the location of the workspace

## Building the project (Runc)
1. Add `export GOPATH=$(pwd)/hs-libcontainer/src/go
2. Create `src`, `pkg`, `bin` at the root of `$GOPATH`
3. `mkdir -p github.com/opencontainers/runc`
4. clone runc at `github.com/opencontainers/runc`

# 2018/6/9
- A place for runc has been set up, it is messy and probably needs some restructure later.
- `make` fails under nix-shell if seccomp is enabled (when `BUILDTAGS := seccomp` in the makefile). I have disabled it for now by setting `BUILDTAGS := ""`.
- runc/libcontainer/configs has some structs that would be a good starting point to bind into c.

Readings:
- [cgo syntax](https://golang.org/cmd/cgo/)
- [cgo goDoc](https://godoc.org/github.com/chai2010/cgo)
- [go to haskell](https://sakshamsharma.com/2018/02/haskell-golang-ffi/#golang-to-c)
- [go to C](https://medium.com/learning-the-go-programming-language/calling-go-functions-from-other-languages-4c7d8bcc69bf)
