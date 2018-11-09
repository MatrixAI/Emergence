{-# LANGUAGE ForeignFunctionInterface #-}

module Lib(create) where

import Commands (CreateCommand)
import Foreign.Ptr (Ptr)

foreign import ccall create :: Ptr CreateCommand -> IO Int
