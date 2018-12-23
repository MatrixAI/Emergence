module Runtime.Types where

import Crypto.Multihash as MH
import Data.Aeson as Aeson
import Data.ByteString (ByteString, append)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (toStrict)
import OCI.RuntimeSpec
import System.FilePath

-- An artifact is a nix store path containing the root file system for a container and 
-- also a runtime configuration in the case of OCI artifacts
data Artifact = NixArtifact { path :: FilePath }
              | OCIArtifact { path :: FilePath 
                            , spec :: RuntimeSpec }
                deriving (Show)

-- An automaton is a pair of configuration and root file system for a container
data Automaton = Automaton Artifact RuntimeSpec
                 deriving (Show)

-- A container is a running instance of an automaton
data Container = Container String FilePath
                 deriving (Show)

class Addressable a where
  addr :: a -> ByteString
  
instance Addressable Artifact where
  addr (NixArtifact path) = (pack . takeWhile (/= '-') . takeBaseName) path

instance Addressable RuntimeSpec where
  addr = (MH.encode' MH.Base32) . (MH.multihash MH.SHA256) . toStrict . Aeson.encode

instance Addressable Automaton where
  addr (Automaton artifact spec) = MH.encode' MH.Base32 $ MH.multihash MH.SHA256 $ append (addr artifact) (addr spec)