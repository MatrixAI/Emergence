{-# LANGUAGE OverloadedStrings #-}

module Runtime.StorageDriver 
  ( mount
  , umount
  , StorageDriver(..)) where
    
import Data.Aeson (encodeFile)
import Data.ByteString.Char8 (unpack)
import Runtime.Types
import System.Directory
import System.FilePath
import System.Process.Typed

data StorageDriver = OverlayFS | ZFS deriving (Show)

mount :: Automaton -> StorageDriver -> IO FilePath
mount a@(Automaton artifact spec) OverlayFS = do
  createDirectoryIfMissing True mountdir
  createDirectoryIfMissing False confdir
  createDirectoryIfMissing False upperdir
  createDirectoryIfMissing False workdir
  createDirectoryIfMissing False mergedir
  encodeFile (confdir ++ "/config.json") spec
  runProcess (shell $ "sudo mount -t overlay overlay -o lowerdir=" ++ lowerdirs ++ ",upperdir=" ++ upperdir 
    ++ ",workdir=" ++ workdir ++ " " ++ mergedir) >>= print
  return mergedir
  where mountdir = "/tmp/" ++ unpack (addr a)
        confdir = mountdir ++ "/conf"
        lowerdirs = path artifact ++ ":" ++ confdir
        upperdir = mountdir ++ "/upper"
        workdir = mountdir ++ "/work"
        mergedir = mountdir ++ "/merged"
mount _ _ = error "Not implemented yet"

umount :: FilePath -> StorageDriver -> IO ()
umount path OverlayFS = do
  runProcess (shell $ "sudo umount " ++ path) >>= print
umount _ _ = error "Not implemented yet"