{-# LANGUAGE OverloadedStrings #-}

module Runtime.Runc where
    
import System.Process.Typed
import Runtime.Types

runcCreate :: Container -> IO ()
runcCreate a = do
  runProcess (shell $ "cd " ++ (path a) ++ " && sudo runc create " ++ (show $ uid a)) >>= print 

runcStart :: Container -> IO ()
runcStart a = do 
  runProcess (shell $ "sudo runc start " ++ (show $ uid a)) >>= print

runcKill :: Container -> Maybe String -> IO ()
runcKill a (Just b) = do
  runProcess (shell $ "sudo runc kill " ++ (show $ uid a) ++ " " ++ b) >>= print
runcKill a Nothing = do
  runProcess (shell $ "sudo runc kill " ++ (show $ uid a)) >>= print

runcDelete :: Container -> IO ()
runcDelete a = do
  runProcess (shell $ "sudo runc delete " ++ (show $ uid a)) >>= print

runcRun :: Container -> IO ()
runcRun a = do 
  runProcess (shell $ "cd " ++ (path a) ++ " && sudo runc run " ++ (show $ uid a)) >>= print 
