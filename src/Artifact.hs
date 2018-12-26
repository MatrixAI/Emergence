{-# LANGUAGE OverloadedStrings #-}

module Artifact where

import Data.Aeson
import Data.Text
import Foreign.Nix.Shellout
import Foreign.Nix.Shellout.Types
import OCI.RuntimeSpec
import Runtime.Types

data OCIImage = OCIImage { imageName :: Text 
                        , imageDigest :: Text
                        , sha256 :: Text }
                deriving (Show)

ociArtifactExpr :: OCIImage -> Text
ociArtifactExpr (OCIImage imageName imageDigest sha256) = "\
\  with import <nixpkgs> {};\
\  let\
\    name = builtins.replaceStrings [\"/\" \":\"] [\"-\" \"-\"] \"oci-artifact-" <> imageName <> "\";\
\    oci-image-tool = pkgs.buildGoPackage rec {\
\      name = \"oci-image-tools-${version}\";\
\      version = \"1.0.0-rc1\";\

\      goPackagePath = \"github.com/opencontainers/image-tools\";\
\      subPackages = [ \"cmd/oci-image-tool\" ];\

\      src = fetchFromGitHub {\
\      owner = \"opencontainers\";\
\      repo = \"image-tools\";\
\      rev = \"v${version}\";\
\      sha256 = \"0c4n69smqlkf0r6khy9gbg5f810qh9g8jqsl9kibb0dyswizr14r\";\
\      };\
\    };\
\  in\
\  runCommand name {\
\    impureEnvVars = pkgs.stdenv.lib.fetchers.proxyImpureEnvVars;\
\    outputHashMode = \"recursive\";\
\    outputHashAlgo = \"sha256\";\
\    outputHash = \"" <> sha256 <> "\";\

\    nativeBuildInputs = [ pkgs.skopeo oci-image-tool ];\
\    SSL_CERT_FILE = \"${pkgs.cacert.out}/etc/ssl/certs/ca-bundle.crt\";\

\    sourceURL = \"docker://" <> imageName <> "@" <> imageDigest <> "\";\
\  } ''\
\    mkdir image\n\
\    skopeo --override-os linux --override-arch amd64 copy \"$sourceURL\" \"oci:image\"\n\

\    mkdir $out\n\
\    mkdir $out/rootfs\n\
\    oci-image-tool create --ref platform.os=linux image/ $out/\n\
\  ''"

pullOCIImage :: OCIImage -> IO (Either (Text, NixError) (StorePath Realized))
pullOCIImage = runNixAction . parseInstRealize . ociArtifactExpr

parseConfig :: StorePath Realized -> IO (Maybe RuntimeSpec)
parseConfig (StorePath path) = do 
  decodeFileStrict' $ path ++ "/config.json"

