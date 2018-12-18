{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OCI.ImageSpec where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Casing
import qualified Data.Map as Map

-- Image is the JSON structure which describes some basic information about the image.
-- This provides the `application/vnd.oci.image.config.v1+json` mediatype when marshalled to JSON.
data ImageSpec = ImageSpec {
  -- Created is the combined date and time at which the image was created, formatted as defined by RFC 3339, section 5.6.
  imagespec_Created :: Maybe String,
  -- Author defines the name and/or email address of the person or entity which created and is responsible for maintaining the image.
  imagespec_Author :: Maybe String,
  -- Architecture is the CPU architecture which the binaries in this image are built to run on.
  imagespec_Architecture :: String,
  -- OS is the name of the operating system which the image is built to run on.
  imagespec_Os :: String,
  -- Config defines the execution parameters which should be used as a base when running a container using the image.
  imagespec_Config :: Maybe ImageConfig,
  -- RootFS references the layer content addresses used by the image.
  imagespec_Rootfs :: RootFS,
  -- History describes the history of each layer.
  imagespec_History :: Maybe [History]
} deriving (Show, Generic)

instance ToJSON ImageSpec where
  toJSON = genericToJSON (aesonPrefix camelCase) { omitNothingFields = True }
instance FromJSON ImageSpec where
  parseJSON = genericParseJSON (aesonPrefix camelCase) { omitNothingFields = True }

defaultImageSpec = ImageSpec { imagespec_Created = Nothing
                             , imagespec_Author = Nothing
                             , imagespec_Architecture = "amd64"
                             , imagespec_Os = "linux"
                             , imagespec_Config = Just defaultImageConfig
                             , imagespec_Rootfs = defaultRootFS
                             , imagespec_History = Nothing }

-- ImageConfig defines the execution parameters which should be used as a base when running a container using an image.
data ImageConfig = ImageConfig {
  -- User defines the username or UID which the process in the container should run as.
  imageconfig_User :: Maybe String,
  -- ExposedPorts a set of ports to expose from a container running this image.
  imageconfig_ExposedPorts :: Maybe (Map.Map String ()),
  -- Env is a list of environment variables to be used in a container.
  imageconfig_Env :: Maybe [String],
  -- Entrypoint defines a list of arguments to use as the command to execute when the container starts.
  imageconfig_Entrypoint :: Maybe [String],
  -- Cmd defines the default arguments to the entrypoint of the container.
  imageconfig_Cmd :: Maybe [String],
  -- Volumes is a set of directories describing where the process is likely write data specific to a container instance.
  imageconfig_Volumes :: Maybe (Map.Map String ()),
  -- WorkingDir sets the current working directory of the entrypoint process in the container.
  imageconfig_WorkingDir :: Maybe String,
  -- Labels contains arbitrary metadata for the container.
  imageconfig_Labels :: Maybe (Map.Map String String),
  -- StopSignal contains the system call signal that will be sent to the container to exit.
  imageconfig_StopSignal :: Maybe String
} deriving (Show, Generic)

instance ToJSON ImageConfig where
  toJSON = genericToJSON (aesonPrefix pascalCase) { omitNothingFields = True }
instance FromJSON ImageConfig where
  parseJSON = genericParseJSON (aesonPrefix pascalCase) { omitNothingFields = True }

defaultImageConfig = ImageConfig { imageconfig_User = Nothing
                                 , imageconfig_ExposedPorts = Nothing 
                                 , imageconfig_Env = Just [ "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" ]
                                 , imageconfig_Entrypoint = Nothing
                                 , imageconfig_Cmd = Just [ "sh" ]
                                 , imageconfig_Volumes = Nothing
                                 , imageconfig_WorkingDir = Just "/"
                                 , imageconfig_Labels = Nothing
                                 , imageconfig_StopSignal = Nothing }

-- RootFS describes a layer content addresses
data RootFS = RootFS {
  -- Type is the type of the rootfs.
  rootfs_Type :: String,
  -- DiffIDs is an array of layer content hashes (DiffIDs), in order from bottom-most to top-most.
  rootfs_DiffIds :: [String]
} deriving (Show, Generic)

instance ToJSON RootFS where
  toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON RootFS where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

defaultRootFS = RootFS { rootfs_Type = "layers"
                       , rootfs_DiffIds = [] }

-- History describes the history of a layer.
data History = History {
  -- Created is the combined date and time at which the layer was created, formatted as defined by RFC 3339, section 5.6.
  history_Created :: Maybe String,
  -- CreatedBy is the command which created the layer.
  history_CreatedBy :: Maybe String,
  -- Author is the author of the build point.
  history_Author :: Maybe String,
  -- Comment is a custom message set when creating the layer.
  history_Comment :: Maybe String,
  -- EmptyLayer is used to mark if the history item created a filesystem diff.
    history_EmptyLayer :: Maybe Bool
} deriving (Show, Generic)

instance ToJSON History where
  toJSON = genericToJSON (aesonPrefix snakeCase) { omitNothingFields = True }
instance FromJSON History where
  parseJSON = genericParseJSON (aesonPrefix snakeCase) { omitNothingFields = True }