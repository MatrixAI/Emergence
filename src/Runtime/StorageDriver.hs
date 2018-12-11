{-# LANGUAGE OverloadedStrings #-}

module Runtime.StorageDriver 
  ( StorageDriver
  , mount
  , umount
  , OverlayFS(..)
  , ZFS(..) ) where
    
import System.Process.Typed
import Runtime.Types

class StorageDriver a where 
  mount :: a -> Overlay -> IO String
  umount :: a -> IO ()

data OverlayFS = OverlayFS String deriving (Show)

instance StorageDriver OverlayFS where 
  mount (OverlayFS mountdir) (OCIOverlay path) = do
    runProcess (shell $ "mkdir " ++ mountdir) >>= print
    runProcess (shell $ "mkdir " ++ upperdir) >>= print
    runProcess (shell $ "mkdir " ++ workdir) >>= print
    runProcess (shell $ "mkdir " ++ mergedir) >>= print
    runProcess (shell $ "sudo mount -t overlay overlay -o lowerdir=" ++ path ++ ",upperdir=" ++ upperdir 
      ++ ",workdir=" ++ workdir ++ " " ++ mergedir) >>= print
    return mergedir
    where upperdir = mountdir ++ "/upper"
          workdir = mountdir ++ "/work"
          mergedir = mountdir ++ "/merged"
  mount _ _ = error "Not implemented yet"

  umount (OverlayFS mountdir) = do 
    runProcess (shell $ "sudo umount " ++ mountdir ++ "/merged") >>= print

data ZFS = ZFS deriving (Show)