{
  pkgs ? import ./pkgs.nix
}:
  with pkgs;
  stdenv.mkDerivation {
    name = "linux-namespace-demo";
    buildInputs = [];
  }