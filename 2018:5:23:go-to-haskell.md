gotools contains compiling options for compiling go code to a c shared library.

[Go to Haskell](https://medium.com/learning-the-go-programming-language/calling-go-functions-from-other-languages-4c7d8bcc69bf)


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
```

```
