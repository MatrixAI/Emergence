{-# LANGUAGE ForeignFunctionInterface #-}

module Lib(foo) where

import Data (InputStruct(..), ReturnStruct(..))
import Foreign.Ptr

foreign import ccall foo :: Ptr InputStruct -> Ptr ReturnStruct -> IO ()

