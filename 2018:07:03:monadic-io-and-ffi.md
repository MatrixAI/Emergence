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
