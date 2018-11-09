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

data Container = Container { cId :: Int
                           , cPath :: String
                           } deriving (Show)

runcCreate :: Container -> IO ()
runcCreate a = do
  runProcess (shell $ "cd " ++ (cPath a) ++ " && sudo runc create " ++ (show $ cId a)) >>= print 

runcStart :: Container -> IO ()
runcStart a = do 
  runProcess (shell $ "runc start " ++ (show $ cId a)) >>= print

runcKill :: Container -> Maybe String -> IO ()
runcKill a (Just b) = do
  runProcess (shell $ "runc kill " ++ (show $ cId a) ++ " " ++ b) >>= print
runcKill a Nothing = do
  runProcess (shell $ "runc kill " ++ (show $ cId a)) >>= print

runcDelete :: Container -> IO ()
runcDelete a = do
  runProcess (shell $ "runc delete " ++ (show $ cId a)) >>= print
