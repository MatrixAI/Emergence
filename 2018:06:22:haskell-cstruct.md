Here's a code snippet on how to turn a haskell record into a c struct.

First we need to ensure that our *cstruct* is a Storable datatype, which means we must implement `sizeOf, alignment, (peek | peekByteOff | peekElemOff), (poke | pokeByteOff | pokeElemOff)`.


- `peek :: GHC.Ptr.Ptr a -> IO a`
	- Read some memory at the given memory location
- `poke :: GHC.Ptr.Ptr a -> a -> IO ()`
	- Write some memory at the given memory location
- `sizeOf` is pretty much equivalent to c's sizeof() fucntion.
- `alignment`: A n-bit alignment = n/8-byte alignment = address space. i.e. a 64bit address space means the 64-bit alignment.

- Both `CInt` and `Int` are in the `Num` type class, this means that convertion between those two class are done automatically
- `CInt` seems to be 4 bytes in 32bit machines and 8 in 64bit machines, similar to `int` in C. (lower bounded 32bits)
- `CChar` is equivalant to `char` in C, which is one byte.
- Use `Foregin.C.String.castCharToCChar` to cast a haskell char to `CChar`.


```haskell
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Foreign.C.String (castCharToCChar)
import Foreign.C.Types (CInt, CChar)
import Foreign.Marshal.Utils (new)

-- +
-- *

-- tuple
-- either

-- all haskell datatypes are sums of products

data MyStructType = MyStructType
 {
   myint :: CInt,
   mychar :: CChar
 } deriving (Show, Read, Eq)

type MyStruct = Ptr MyStructType

instance Storable MyStructType where
 sizeOf _ = 8
 alignment = sizeOf
 peek ptr = do
   i <- peekByteOff ptr 0
   c <- peekByteOff ptr 4
   return $ MyStructType i c
 poke p (MyStructType i c) = do
   pokeByteOff p 0 i
   pokeByteOff p 4 c

{-

struct MyStructType {
 int32_t myint;
 unsigned char mychar;
};


-}

mystruct :: IO (Ptr MyStructType)
mystruct = new $ MyStructType 1 (castCharToCChar 'a')

mystruct2 :: IO MyStructType
mystruct2 = do
 p <- mystruct
 s <- peek p
 return s


printit :: IO ()
printit = mystruct2 >>= print

-- result :: IO ()
-- result = do
--   p <- mystruct
--   s <- peek p
--   print s
```
