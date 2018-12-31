module Runtime.Network where

import System.Process.Typed

initNetwork :: String -> IO ()
initNetwork pid = do
  runProcess_ $ proc "sudo" ["ln", "-s", nsFile, nsLink]
  runProcess_ $ shell "sudo ip link add host type veth peer name container"
  runProcess_ $ shell $ "sudo ip link set netns " ++ pid ++ " dev container"
  runProcess_ $ shell "sudo ip link set up dev host"
  runProcess_ $ shell "sudo ip addr add 192.168.113.1/24 dev host"
  runProcess_ $ shell $ "sudo ip -n " ++ pid ++ " link set up container"
  runProcess_ $ shell $ "sudo ip -n " ++ pid ++ " addr add 192.168.113.2/24 dev container"
  where nsFile = "/proc/" ++ pid ++ "/ns/net"
        nsLink = "/var/run/netns/" ++ pid

clearNetwork :: String -> IO ()
clearNetwork pid = do
  runProcess_ $ shell $ "sudo ip netns del " ++ pid
  runProcess_ $ shell "sudo ip link del host"