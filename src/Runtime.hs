module Runtime where 

import Runtime.Runc
import Runtime.StorageDriver
import Runtime.Types
import Runtime.Network
import OCI.RuntimeSpec

deployAndRun :: String -> RuntimeSpec -> Artifact -> IO ()
deployAndRun name spec artifact = do
  let a = Automaton artifact spec
  b <- mount a OverlayFS
  runcRun name b
  umount b OverlayFS

deploy :: RuntimeSpec -> Artifact -> IO FilePath
deploy spec artifact = mount (Automaton artifact spec) OverlayFS

create :: String -> FilePath -> IO Container
create name path = do 
  runcCreate name path
  pid <- readFile (path ++ "/pidFile") 
  return $ Container name pid

setup :: Container -> IO ()
setup = initNetwork . pid

start :: Container -> IO ()
start = runcStart . cid

stop :: Container -> IO ()
stop c = runcKill (cid c) Nothing

delete :: Container -> IO ()
delete = runcDelete . cid

cleanup :: Container -> IO ()
cleanup = clearNetwork . pid