{-# LANGUAGE ForeignFunctionInterface #-}

module Test(strFxn) where

import Foreign.C.String
import Foreign.Marshal.Alloc

foreign import ccall "StrFxn" go_StrFxn :: CString -> IO CString

strFxn :: String -> IO String
strFxn input = do 
    cinput <- newCString input
    coutput <- go_StrFxn cinput
    res <- peekCString coutput
    _ <- free cinput
    _ <- free coutput
    return res
