{
  pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/7cbf6ca1c84dfc917c1a99524e082fb677501844.tar.gz) {}
}:
  with pkgs;
  haskell.lib.buildStackProject {
    name = "container-demo";
    buildInputs = [go libseccomp];
    shellHook = ''
      echo 'Building container demo'
      set -v
      alias stack="\stack --nix"
      set +v
      export GOPATH=$(pwd)/src/godeps
      go get github.com/davecgh/go-spew/spew
      go get github.com/matrixai/runc      
    '';
  }
