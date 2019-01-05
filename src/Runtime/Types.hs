module Runtime.Types where

import Crypto.Multihash as MH
import Data.Aeson as Aeson
import qualified Data.ByteString as BS (ByteString, append)
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Lazy as BS (toStrict)
import qualified Data.Text as T
import Foreign.Nix.Shellout.Types
import OCI.RuntimeSpec
import System.FilePath

-- A filesystem specification contains all necessary input to build the root filesystem of an artifact.
data FSConfig = OCIFSConfig { imageName :: T.Text 
                            , imageDigest :: T.Text
                            , sha256 :: T.Text }
              | NixFSConfig { name :: T.Text 
                            , contents :: [T.Text]
                            , extraContents :: [(T.Text, T.Text)] }
                deriving (Show)

-- A config contains all necessary input to deploy and execute an artifact.
data RuntimeConfig = RuntimeConfig deriving (Show)

-- An artifact is a nix store path containing the root file system for a container and 
-- also a runtime configuration in the case of OCI artifacts
data Artifact = Artifact { path :: FilePath 
                         , spec :: Maybe RuntimeSpec }
                deriving (Show)

-- An automaton is a pair of configuration and root file system for a container
data Automaton = Automaton Artifact RuntimeSpec
                 deriving (Show)

-- A container is a running instance of an automaton
data Container = Container { cid :: String
                           , pid :: String }
                 deriving (Show)

class Addressable a where
  addr :: a -> BS.ByteString
  
instance Addressable Artifact where
  addr (Artifact path _) = (BS.pack . takeWhile (/= '-') . takeBaseName) path

instance Addressable RuntimeSpec where
  addr = (MH.encode' MH.Base32) . (MH.multihash MH.SHA256) . BS.toStrict . Aeson.encode

instance Addressable Automaton where
  addr (Automaton artifact spec) = MH.encode' MH.Base32 $ MH.multihash MH.SHA256 $ BS.append (addr artifact) (addr spec)