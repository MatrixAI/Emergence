{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    , someFunc'
    ) where

import Artifact
import Runtime
import Runtime.Types
import OCI.RuntimeSpec

someFunc :: IO ()
someFunc = do 
  nixHello >>= deployAndRun "my-hello" defaultRuntimeSpec {
    process = Just defaultProcess { terminal = Just False, args = [ "hello" ] }, mounts = Just defaultMountsWithNixStore }

someFunc' :: IO ()
someFunc' = do 
  ociHello >>= deployAndRun "my-hello" defaultRuntimeSpec { process = Just defaultProcess { terminal = Just False, args = [ "/hello" ] } }

nixHello :: IO Artifact
nixHello = buildArtifact' $ NixFSConfig "hello" ["hello", "bash", "coreutils"] []

ociHello :: IO Artifact
ociHello = buildArtifact' $ OCIFSConfig "hello-world" digest sha256
  where digest = "sha256:0add3ace90ecb4adbf7777e9aacf18357296e799f81cabc9fde470971e499788" 
        sha256 = "0pnhzsgmm23knzfp6jadpq5yyrhxz32y5nnlj1q3ryqzydg16f9d"

sampleNginx :: IO ()
sampleNginx = nixNginx >>= deployAndRun "my-nginx" defaultRuntimeSpec { 
    process = Just defaultProcess { terminal = Just False
                                  , args = [ "nginx", "-c", "/etc/nginx.conf" ] 
                                  }, 
    mounts = Just $ Mount { destination = "/usr/share/nginx/html"
                          , m_type = Just "none"
                          , source = Just "/tmp/html"
                          , options = Just ["bind", "ro"]
                          } : defaultMountsWithNixStore 
  }

nixNginx :: IO Artifact 
nixNginx = buildArtifact' $ NixFSConfig "nginx" ["nginx", "bash", "coreutils"] [("nginxConf", nginxConf)]
  where nginxConf = "\
\    writeTextFile {\n\
\      name = \"nginx-conf\";\n\
\      text = ''\n\
\        error_log /tmp/logs;\n\
\        daemon off;\n\
\      ''; \n\
\      destination = \"/etc/nginx.conf\";\n\
\   }"

