module Main where

import Commands (BaseCommand(..), RunnableCommand(..), CreateCommand(..))
import Foreign.Ptr (Ptr)
import Foreign.C.String (newCAString)
import Foreign.Marshal.Utils (new)
import Lib (create)

defaultBase :: IO (Ptr BaseCommand)
defaultBase = do
  statePath <- newCAString "run/runc"
  criu <- newCAString "criu"
  new $ BaseCommand statePath criu True 2

defaultRunnable :: IO (Ptr RunnableCommand)
defaultRunnable = do
  base <- defaultBase
  containerID <- newCAString "cont01"
  notifySocket <- newCAString ""
  new $ RunnableCommand base containerID False False notifySocket

defaultCreate :: IO (Ptr CreateCommand)
defaultCreate = do
  runnable <- defaultRunnable
  bundle <- newCAString "/home/moku/matrix-ai/Emergence/practices/hs-libcontainer/test/mycontainer"
  consoleSocket <- newCAString ""
  pidFile <- newCAString ""
  new $ CreateCommand runnable bundle consoleSocket pidFile 0

main :: IO ()
main =  do
  status <- defaultCreate >>= create
  print status
