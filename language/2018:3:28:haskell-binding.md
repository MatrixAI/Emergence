# Writing Haskell Binding

## Which preprocessor should we use
Haskell FFI interface makes it possible to written Haskell bindings without specifying any special preprocessors. However it is recommended to use a FFI supportive preprocessor such as c2h2 and hsc2hs for the ease of development and maintenance.

**c2hs** is a Haskell preprocessor that helps generate FFI bindings, along with **hsc2hs** and **GreenCard**. c2h2 contains some features that are not supported in hsc2hs, but using it also requires reading more documentations etc. Generally, c2h2 is recommended for larger projects whereas hsc2hs is recommended for smaller projects. GreenCard is a more recent preprocessor that has gained some good trust, but is relatively less known compared to the former two.

Features that c2h2 includes:
- Port Enum definitions into pure Haskell code.
- Automatically compute the sizes of structures you are marshalling to and from.
- Automatically compute the offsets to peek and poke at fields in structures.
- (To some extent) writing the actual `foreign import` declarations for C functions you want to use.
- Automatic generation of `foreign import` based on the content of the C header file *(unsupported in hsc2hs)*
- Semi-automatic marshalling to and from function calls *(unsupported in hsc2hs)*
- Translation of pointer types and hierarchies into Haskell types *(unsupported in hsc2hs)*

## Including C Files
### Smaller Projects
For smaller projects where we have the c code, we can modify the cabal file as such and make it do the linking:

```cabal
library
  -- The C source file to compile in order
  C-sources: cbits/foo.c cbits/bar.c
  -- Location of the header files
  Include-dirs: include
  -- The header files to be included (optional)
  Includes: foo.h bar.h
  -- Header files to install
  Install-includes: foo.h
```

By convention, we put the C source files in the `cbits` directory, and header files in the `include` directory. The `Includes` field tells Cabal to go and check that those include files exist and are usable prior to compilation, which gives a better error message at compile time. The `Install-includes` field causes Cabal to place those header files in a public location upon installation. This is necessary for older versions of GHC or if modules that use your module need to perform C includes of your library or cbits.

### Larger Projects
For larger scale projects, we might want to use the traditional `configure && make` process. In this case, we can edit the `Setup.hs` file:

```
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit)

main = defaultMainWithHooks simpleUserHooks
    { preBuild = \a b -> makeLib a b >> preBuild simpleUserHooks a b }

makeLib :: Args -> BuildFlags -> IO ()
makeLib _ flags =
    rawSystemExit (fromFlag $ buildVerbosity flags) "env"
        ["CFLAGS=-D_LIB", "make", "--directory=abc", "libabc.a"]
```
[Source](http://blog.ezyang.com/2010/06/setting-up-cabal-the-ffi-and-c2hs/)

This didn't work very well for me, I'll come back to this at a later time.

## FFI API Design Principles
### Low-level Design
- Naming conventions
- Marshalling vanilla C values (e.g. `int`, `float` and `char*` for null terminated string) into (`Int`, `Float` and `String`).
- Converting `int` into `Bool` from some naming convention.
- Putting `malloc`'d pointers into memory management with foreign pointers. **IMPORTANT**
- Converting functions that initialise some memory space into pure versions with `unsafePerformIO`, `alloca` and `peek`. Note that this should be used in conjunction with the appropriate `Storable` instance to marshal the C struct into persistent Haskell datatype.
- Marshalling more complex C values (arrays, mostly) into Haskell lists, assuming that bounds information is consistent and available locally.

The [blog post](http://blog.ezyang.com/2010/06/principles-of-ffi-api-design/) that I'm reading suggests to marshal flat structs only (structs that contain no pointers) and let everything else be for efficiency. Understanding how inefficient it is may require me to understand how the marshalling process works from C to Haskell/Haskell to C. But for now I will take the author's word for it.

### High-level Design
- **Pure functions**. Functions that are referentially transnparent can be transformed into pure functions easily. A set of internal state transformation functions might not be amendable to a pure treatment, but perhaps a function that orchestrates them together leaks no shared state.
- **Monads**. It is a good design decision to give your users a more restrictive monads that can perform operations relevant to your library. This can be done simple like the following:
  ```Haskell
  newtype MyMonad a = MyMonad { unMyMonad :: ReaderT Env IO a }
    deriving (MonadReader Env, Monad, Functor)
  ```
  

## Sources
[c2h2 and FFI tutorial](http://blog.ezyang.com/2010/06/the-haskell-preprocessor-hierarchy/)
