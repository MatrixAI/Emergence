{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import System.Process.Typed

someFunc :: IO ()
someFunc = putStrLn "someFunc"

someProc :: IO ()
someProc = do
  runProcess "true" >>= print
  runProcess "false" >>= print
