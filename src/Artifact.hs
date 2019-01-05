{-# LANGUAGE OverloadedStrings #-}

module Artifact where

import Data.Aeson
import qualified Data.Text as T
import Foreign.Nix.Shellout
import Foreign.Nix.Shellout.Types
import OCI.RuntimeSpec
import Runtime.Types

artifactNixExpr :: FSConfig -> T.Text
artifactNixExpr (OCIFSConfig imageName imageDigest sha256) = "\
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

\    nativeBuildInputs = [ pkgs.skopeo oci-image-tool jq moreutils ];\
\    SSL_CERT_FILE = \"${pkgs.cacert.out}/etc/ssl/certs/ca-bundle.crt\";\

\    sourceURL = \"docker://" <> imageName <> "@" <> imageDigest <> "\";\
\  } ''\
\    mkdir image\n\
\    skopeo --override-os linux --override-arch amd64 copy \"$sourceURL\" \"oci:image\"\n\

\    mkdir $out\n\
\    mkdir $out/rootfs\n\
\    oci-image-tool create --ref platform.os=linux image/ $out/\n\

\    manifestJson=image/blobs/$(jq -r '.manifests[].digest' image/index.json | sed 's/:/\//')\n\
\    configJson=image/blobs/$(jq -r '.config.digest' $manifestJson | sed 's/:/\//')\n\
\    exposedPorts=$(jq '.config.ExposedPorts | keys | join(\",\")' $configJson)\n\
\    jq --arg exposedPorts $exposedPorts'.annotations.\"org.opencontainers.image.exposedPorts\" = $exposedPorts' $out/config.json | sponge config.json\n\
\  ''"

artifactNixExpr (NixFSConfig name contents extras) = "\
\  with import <nixpkgs> {};\n\
\  let\n\
\    name = builtins.replaceStrings [\"/\" \":\"] [\"-\" \"-\"] \"nix-artifact-" <> name <> "\";\n\
\    contents = [" <> (T.unwords $ contents ++ (fmap fst extras)) <> "];\n\
\    " <> foldr (\a b -> a <> ";\n" <> b) "" (fmap (\(a,b) -> a <> " = " <> b) extras) <> "\
\  in\n\
\    runCommand name { \n\
\      inherit contents;\n\
\      nativeBuildInputs = [ pkgs.jq ];\n\
\    }\n\
\    ''\n\
\    mkdir $out\n\
\    mkdir $out/rootfs\n\

\    if [[ -n \"$contents\" ]]; then\n\
\      echo \"Adding contents...\"\n\
\      for item in $contents; do\n\
\        echo \"Linking $item\"\n\
\        find $item -mindepth 1 -type d | cut -d/ -f5- | while read dir; do mkdir -p \"$out/rootfs/$dir\"; done\n\
\        find $item -not -type d | cut -d/ -f5- | while read file; do ln -s \"$item/$file\" \"$out/rootfs/$file\"; done\n\
\      done\n\
\    else\n\
\      echo \"No contents to add to layer.\"\n\
\    fi\n\

\    echo \"Finished building nix artifact '${name}'\"\n\
\    ''"


buildArtifact :: FSConfig -> IO (Either (T.Text, NixError) Artifact)
buildArtifact fs = 
  let 
    buildFS = runNixAction . parseInstRealize . artifactNixExpr
    parseConfig = decodeFileStrict' . (++ "/config.json") . fromStorePath
  in do 
    res <- buildFS fs
    conf <- case fs of 
      (OCIFSConfig _ _ _) -> either (\_ -> return Nothing) parseConfig res
      (NixFSConfig _ _ _) -> return Nothing
    return $ fmap (\(StorePath path) -> Artifact path conf) res
  
buildArtifact' :: FSConfig -> IO Artifact
buildArtifact' fs = do 
  res <- buildArtifact fs
  return $ either (error . T.unpack . fst) id res