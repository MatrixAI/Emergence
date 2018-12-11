module Lib
    ( someFunc
    ) where

import Runtime

someFunc :: IO ()
someFunc = do 
    deployAndRun "/nix/store/c0yss9m9iszhb3xp4b5hqd7np9i1n0z1-oci-bundle-hello-bundle" "my-hello"