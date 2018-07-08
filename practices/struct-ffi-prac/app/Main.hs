module Main where

import Lib(foo)
import Data(InputStruct(..), ReturnStruct(..))
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable

input :: IO (Ptr InputStruct)
input = new $ InputStruct 12

output :: IO (Ptr ReturnStruct)
output = new $ InputStruct 0 -- Set to some default value

main :: IO ()
main = do
    inp <- input
    oup <- output
    foo inp oup
    peek oup >>= print
