{
  pkgs ? import <nixpkgs> {} 
}:
  with pkgs;
  let
    image-tools = import ./image-tools.nix {};
  in
    stdenv.mkDerivation {
      name = "image-tools-demo";
      buildInputs = [ image-tools ];
    }
