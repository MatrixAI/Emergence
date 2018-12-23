{-# LANGUAGE OverloadedStrings #-}

module Runtime.Runc where
    
import System.Process.Typed
import Runtime.Types hiding (path)

runcCreate :: Container -> IO ()
runcCreate (Container cid path) = do
  runProcess (shell $ "cd " ++ path ++ " && sudo runc create " ++ cid) >>= print 

runcStart :: Container -> IO ()
runcStart (Container cid _) = do 
  runProcess (shell $ "sudo runc start " ++ cid) >>= print

runcKill :: Container -> Maybe String -> IO ()
runcKill (Container cid _) (Just sig) = do
  runProcess (shell $ "sudo runc kill " ++ cid ++ " " ++ sig) >>= print
runcKill (Container cid _) Nothing = do
  runProcess (shell $ "sudo runc kill " ++ cid) >>= print

runcDelete :: Container -> IO ()
runcDelete (Container cid _) = do
  runProcess (shell $ "sudo runc delete " ++ cid) >>= print

runcRun :: Container -> IO ()
runcRun (Container cid path) = do 
  runProcess (shell $ "cd " ++ path ++ " && sudo runc run " ++ cid) >>= print 
