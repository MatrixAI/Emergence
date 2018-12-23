module Lib
    ( someFunc
    ) where

import Runtime
import Runtime.Types
import OCI.RuntimeSpec

someFunc :: IO ()
someFunc = do 
    deployAndRun (NixArtifact "/nix/store/jp4gjq1j9m56phl8syi96ajdf8kwhk1z-nix-artifect-hello") defaultRuntimeSpec {
         process = Just defaultProcess { terminal = Just False, args = [ "hello" ] }, mounts = Just defaultMountsWithNixStore } "my-hello"