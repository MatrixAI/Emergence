{
  pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/8b1cf100cd8badad6e1b6d4650b904b88aa870db.tar.gz) {}
}:
  with pkgs;
  haskell.lib.buildStackProject {
    name = "hs-libcontainer";
    buildInputs = [haskellPackages.c2hs
		   go];
    shellHook = ''
      echo 'Entering hs-libcontainer Environment'
      set -v

      alias stack='\stack --nix'
      export GOPATH=$(pwd)/src/godeps
     
       set +v
    '';
  }
