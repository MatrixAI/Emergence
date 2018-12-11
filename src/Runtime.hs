module Runtime where 

import Runtime.Runc
import Runtime.StorageDriver
import Runtime.Types

deployAndRun :: String -> String -> IO ()
deployAndRun overlayPath name = do
    let overlayFS = OverlayFS $ "/tmp/" ++ name
    containerPath <- mount overlayFS (OCIOverlay overlayPath)
    runcRun (Container name containerPath)
    umount overlayFS
