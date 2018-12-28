module Lib
    ( someFunc
    ) where

import Artifact
import Runtime
import Runtime.Types
import OCI.RuntimeSpec

someFunc :: IO ()
someFunc = do 
  deployAndRun (Artifact "/nix/store/jp4gjq1j9m56phl8syi96ajdf8kwhk1z-nix-artifect-hello" Nothing) defaultRuntimeSpec {
    process = Just defaultProcess { terminal = Just False, args = [ "hello" ] }, mounts = Just defaultMountsWithNixStore } "my-hello"

nixHello :: IO Artifact
nixHello = buildArtifact $ NixFSConfig "hello" ["hello", "bash", "coreutils"]

ociHello :: IO Artifact
ociHello = buildArtifact $ OCIFSConfig "hello-world" digest sha256
  where digest = "sha256:0add3ace90ecb4adbf7777e9aacf18357296e799f81cabc9fde470971e499788" 
        sha256 = "0pnhzsgmm23knzfp6jadpq5yyrhxz32y5nnlj1q3ryqzydg16f9d"

