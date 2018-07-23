module Main where
--
-- mystruct :: IO (Ptr LinuxIntelRdt)
-- mystruct = do
--   string <- newCAString "Hello"
--   new $ LinuxIntelRdt string
--
-- mystruct2 :: IO LinuxIntelRdt
-- mystruct2 = do
--   p <- mystruct
--   s <- peek p
--   return s

main :: IO ()
-- main = mystruct2 >>= print
main = putStrLn "Hello"
