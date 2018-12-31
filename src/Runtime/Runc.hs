{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Runtime.Runc where
    
import GHC.Generics
import Data.Aeson 
import Data.Word
import System.Process.Typed

runcCreate :: String -> FilePath -> IO ()
runcCreate name path = 
  runProcess_ $ proc "sudo" ["runc", "create", "-b", path, "--pid-file", path ++ "/pidFile", name]
 
runcStart :: String -> IO ()
runcStart name = runProcess_ $ proc "sudo" ["runc", "start", name]

runcKill ::  String -> Maybe String -> IO ()
runcKill name (Just sig) = runProcess_ $ proc "sudo" ["runc", "kill", name, sig]
runcKill name Nothing = runProcess_ $ proc "sudo" ["runc", "kill", name]

runcDelete ::  String -> IO ()
runcDelete name = runProcess_ $ proc "sudo" ["runc", "delete", name]

runcRun ::  String -> FilePath -> IO ()
runcRun name path = 
  runProcess_ $ proc "sudo" ["runc","run", "-b", path, "--pid-file", path ++ "/pidFile", name]

data MemoryConstraints = 
  MemoryConstraints { -- Memory limit (in bytes)
                      limit :: Maybe Int,
                      -- Memory reservation or soft_limit (in bytes)
                      reservation :: Maybe Int,
                      -- Total memory usage (memory + swap); set '-1' to enable unlimited swap
                      swap :: Maybe Int,
                      -- Kernel memory limit (in bytes)
                      kernel :: Maybe Int,
                      -- Kernel memory limit (in bytes) for tcp buffer
                      kernelTcp :: Maybe Int } 
  deriving (Show, Generic)

instance ToJSON MemoryConstraints where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON MemoryConstraints where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }
                      
data CPUConstraints = 
  CPUConstraints { -- CPU shares (relative weight vs. other containers)
                   shares :: Maybe Int,
                   -- CPU CFS hardcap limit (in usecs). Allowed cpu time in a given period
                   quota :: Maybe Int,
                   -- CPU CFS period to be used for hardcapping (in usecs). 0 to use system default
                   period :: Maybe Int,
                   -- CPU realtime hardcap limit (in usecs). Allowed cpu time in a given period
                   realtimeRuntime :: Maybe Int,
                   -- CPU realtime period to be used for hardcapping (in usecs). 0 to use system default
                   realtimePeriod :: Maybe Int,
                   -- CPU(s) to use
                   cpus :: Maybe String,
                   -- Memory node(s) to use
                   mems :: Maybe String }
  deriving (Show, Generic)

instance ToJSON CPUConstraints where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON CPUConstraints where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

data BlockIOConstraints = 
  BlockIOConstraints { -- Specifies per cgroup weight, range is from 10 to 1000 (default: 0)
                     weight :: Maybe Int }
  deriving (Show, Generic)

instance ToJSON BlockIOConstraints where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON BlockIOConstraints where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

data ResourceConstraints = 
  ResourceConstraints { memory :: Maybe MemoryConstraints
                      , cpu :: Maybe CPUConstraints
                      , blockIO :: Maybe BlockIOConstraints }
  deriving (Show, Generic)

instance ToJSON ResourceConstraints where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON ResourceConstraints where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

runcUpdate :: String -> ResourceConstraints -> IO ()
runcUpdate name res = runProcess_ $ setStdin (byteStringInput $ encode res) $ proc "sudo" ["runc", "update", "-r", "-", name]

runcList :: IO ()
runcList = runProcess_ $ proc "sudo" ["runc", "list"]