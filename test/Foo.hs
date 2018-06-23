{-# LANGUAGE ForeignFunctionInterface #-}
module Foo where
 
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
 
-- pure function
foreign export ccall foo :: MyStruct -> Int
 
foo :: MyStruct -> Int
foo = const 42
 
-- impure function
foreign export ccall showStruct :: MyStruct -> IO ()
 
showStruct :: MyStruct -> IO ()
showStruct ss = peek ss >>= print
 
data MyStructType = MyStructType CInt CChar
  deriving (Show, Read, Eq)
type MyStruct = Ptr MyStructType
 
instance Storable MyStructType where
  sizeOf _ = 8
  alignment _ = 8
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 4
    return (MyStructType a b)
