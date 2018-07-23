module Main where

import Commands (BaseCommand, RunnableCommand, CreateCommand)

defaultBase :: IO (Ptr BaseCommand)
defaultBase = do
  statePath <- newCAString "/run/user/1002/runc"
  criu <- newCAString "criu"
  return $ Ptr $ BaseCommand statePath criu False 0

defaultRunnable :: IO (Ptr RunnableCommand)
defaultRunnable = do
  base <- defaultBase
  containerID <- newCAString ""
  notifySocket <- newCAString ""
  return $ RunnableCommand base containerID False False notifySocket 0

--
-- mystruct :: IO (Ptr LinuxIntelRdt)
-- mystruct = do
--   string <- newCAString "Hello"
--   new $ LinuxIntelRdt string
--
-- mystruct2 :: IO LinuxIntelRdt
-- mystruct2 = do
--   p <- mystruct
--   s <- peek p
--   return s

main :: IO ()
-- main = mystruct2 >>= print
main = putStrLn "Hello"
