module Runtime where 

import Runtime.Runc
import Runtime.StorageDriver
import Runtime.Types
import OCI.RuntimeSpec

deployAndRun :: Artifact -> RuntimeSpec -> String -> IO ()
deployAndRun artifact spec name = do
    let a = Automaton artifact spec
    b <- mount a OverlayFS
    runcRun (Container name b)
    umount b OverlayFS
