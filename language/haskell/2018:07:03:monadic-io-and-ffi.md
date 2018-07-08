# Monadic I/O

A value of type `IO a` is an "action" that, when performed, may do some input/ouput before delivering a value of type `a`.

This can be thought of as this type definition:
```haskell
type IO a = World -> (a, World)
```
where the program takes the state of the entire world as its input, and delivers a modified world as a result (due to side effects).

We have a **bind** operator:
```haskell
(>>=) :: IO a -> (a -> IO b) -> IO b
```

It works like pipes in Unix but more complicated. When `a >>= f` is performed, it performs action `a`, takes the result applies `f` to it to get a *new action*, and then performs the new action.

We also have a **then** operator:
```haskell
(>>) :: IO a -> IO b -> IO b
(>>) a1 a2 = a1 >>= (\x -> a2)
```

It performs the first action, throws away the result (using lambda abstraction) and performs the second action.


To return more than one outputs from multiple actions, we can use:
```haskell
return :: a -> IO a
```

The action (return v) is an action that does no I/O. and immediately returns v without having any side effects.

A complete Haskell program defines a single big I/O action, called `main` of type `IO ()`.

The `do` Notation is used for monadic operations. The translation rules are simple:

```haskell
do {x <- e; s} = e >>= \x -> do {s}
do {e; s} = e >> do {s}
do {e} = e
```
`x <- e` binds the variable `x`. It does not *assign to the location `x`* as it would be the case in an imperative program. Hence if we bind to the same variable name twice, we bind two distinct variables.

Using these operators, we can express loops as such:

```haskell
-- Infinite loop
forever :: IO () -> IO ()
forever a = a >> forever a

-- repeat N times
repeatN :: int -> IO a -> IO ()
repeatN 0 a = return ()
repeatN n a = a >> repeatN (n-1) a

-- for loop
for :: [a] -> (a -> IO ()) -> IO ()
for []     fa = return ()
for (n:ns) fa = fa n >> for ns fa
```

We can model mutable objects using references like such:
```haskell
data IORef a -- An abstract type

newIORef    :: a -> IO (IORef a)
readIORef   :: IORef a -> IO a
writeIORef  :: IORef a -> a -> IO ()
```

Haskell98 provides a direct analogy of the Standard C library functions for opening, reading, and writing a file.

```haskell
openFile  :: String -> IOMode -> IO Handle
hPutStr   :: Handle -> [Char] -> IO ()
hGetLine  :: Handle -> IO [Char]
hClose    :: Handle -> IO ()
```

Suppose we want to keep track of how many characters are written to a file, we can arrange hPutStr and hGetLine each increment a mutable variable suitably. We can do this by:

```haskell
type HandleC = (Handle, IORef Int)
```

and we can override the original open/close functions with our own, dealing with `HandleC`

IO monad is an *abstract data type*, that is, a type together with a collection of operations over that type.
- All the operations except one, `(>>=)`, have an I/O action as their *result*, but do not take one as an *argument*.
- The only operation that *combines* I/O actions is `(>>=)`
- The `IO` monad is "sticky": no operation takes argument(s) with an IO type and returns a result with a non-IO type.

the `unsafe` I/O primitive is common in Haskell implementations because sometimes an operation in the class performs some IO operations causes other part of the class to have to use IO.

`unsafePerformIO :: IO a -> a` causes the effected world from the action to be discarded. Whenever you use it, you are primising the compiler that **the timing of this I/O operation, relative to all other I/O operation of the program, does not matter.** This is what the unsafe prefix mean.

Three common usages of unsafe:
- Perform once-per-run IO, as for configFileContents
- Allocating a global mutable variable, e.g.
  ```haskell
  noOfOpenFiles :: IORef Int
  noOfOpenFiles = unsafePerformIO (newIORef 0)
  ```
- Emitting trace messages for debugging purposes:


// TODO: How to make a monad (skipping unless necessary)

# Concurrency
// TODO: Skipping unless necessary

# Interfacing to other programs
To call a C procedure from Haskell, we do:

```haskell
foreign import ccall putChar :: Char -> IO ()
```

Haskell FFI allows omittion of IO from the return type.

```haskell
foreign import ccall sin :: Float -> Float
```

- The keyword `ccall` indicates the calling convention to use.
- If the foreign procedure does not have the same name as the Haskell counterpart. Do this:
  `foreign import ccall "PutChar" putChar :: Char -> IO ()`
- There is a strictly limited range of Haskell types that can be used in arguments and results, namely atomic types such as `Int`, `Float`, `Double` and so on.
- FFI must provide a collection of new atomic types including `Ptr t`, for uninterpreted machine addresses, while type `t` is a "phantom type", which allows Haskell programmers to enforce the distinction between `ptr Foo` and `Ptr Baz`

An **indirect call** to an external procedure means one is supplied with the address of the procedure and one wants to call it, i.e. the address of the procedure is used to perform the function call.

To make an indirect call from Haskell, use the `dynamic` keyword:

```haskell
foreign import ccall "dynamic"
	foo :: FunPtr (Int -> IO Int) -> Int -> IO Int
```

The first argument **must** be `FunPtr t`, the address of the external function.

There is a way to export dynamic Haskell value:

```haskell
foreign import ccall "wrapper"
	mkCB :: (Int -> IO Int) -> IO (FunPtr (int -> IO Int))

-- must be freed using
freeHaskellFunctionPtr :: Addr -> IO ()
```

which returns a C function pointer that can be somehow passed to the C program and gets subsequently called on.


To marshal structured data types, the Haskell community has reached a consensus: "We define a *language extension* that is as small as possible, and build *separate tools* to generate marshalling code."

The `foreign import` and `foreign export` declarations constitute the language extension. 

There are two main issues in FFI memory management:

**Foreign objects**: If a procedure (which allocates resources and returns a handle) is imported into a Haskell program, how do we know when to finalise (release) the handle returned by the procedure?


We can wrap a Ptr in `ForeignPtr` to ensure that the garbage collector calls the finalisation action upon an object when it is no longer accessible.

```haskell
newForeignPtr :: Ptr a -> IO () -> IO (ForeignPtr a) 

-- To unwrap a foreign pointer
withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
```

To unwrap a foreign pointer, we can't simply do `ForeignPtr a -> IO Ptr a` because then the foreign pointer itself might be unreferenced after the unwrapping call and its finaliser might therefore be called before we are done with Ptr.

Example

```haskell
foreign import ccall "and_bmp"
	and_bmp_help :: Ptr Bitmap -> Ptr Bitmap -> IO (Ptr Bitmap)

foreign import ccall free_bmp :: Ptr Bitmap -> IO ()

and_bmp :: ForeignPtr Bitmap -> ForeignPtr Bitmap -> IO (ForeignPtr Bitmap)
and_bmp b1 b2 = withForeignPtr b1 (\p1 ->
		withForeignPtr b2 (\p2 ->
		do { r <- and_bmp_help p1 p2
		     newForeignObj r (free_bmp r)
		} 
```


**Stable pointers**

We cannot simply return a pointer into the Haskell heap because:
- The Haskell garbage collector would not know when the object is no longer required. If the C program holds the only pointer to the object, the collector is likely to treat the object as garbage.
- The haskell collector may move objects around. So the address is not stable

The straightforward, if brutal solution to both of these problems is to provide a way to convert a Haskell value into a stable pointer:

```haskell
newStablePtr	:: a -> IO (StablePtr a)
deRefStablePtr	:: StablePtr a -> IO a
freeStablePtr	:: StablePtr a -> IO ()
```

`newStablePtr` takes an Haskell value and turns it into a stable pointer. 
- It is unaffected by garbage collector. Can be passed to C as a parameter, from the C side, `StablePtr` looks like an `int`. The C program can subsequently pass the stable pointer to a Haskell function, which can get at the original value using `deRefStablePtr`
- calling `newStablePtr` registers the Haskell value as a garbage-collection root, by installing a pointer to `v` in the *Stable Pointer Table (SPT)*. It will remain in SPT unless `freeStablePtr` is called.






