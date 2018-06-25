module Main where

import OCISpec (LinuxIntelRdt(..), l3CacheSchema)
import Foreign.Ptr (Ptr)
import Foreign.C.String (newCAString)
import Foreign.Marshal.Utils (new)
import Foreign.Storable (Storable(..))

mystruct :: IO (Ptr LinuxIntelRdt)
mystruct = do
  string <- newCAString "Hello"
  new $ LinuxIntelRdt string

mystruct2 :: IO LinuxIntelRdt
mystruct2 = do
  p <- mystruct
  s <- peek p
  return s

main :: IO ()
main = mystruct2 >>= print
