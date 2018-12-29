module Runtime where 

import Runtime.Runc
import Runtime.StorageDriver
import Runtime.Types
import OCI.RuntimeSpec

deployAndRun :: String -> RuntimeSpec -> Artifact -> IO ()
deployAndRun name spec artifact = do
    let a = Automaton artifact spec
    b <- mount a OverlayFS
    runcRun (Container name b)
    umount b OverlayFS