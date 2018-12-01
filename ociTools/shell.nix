{
  pkgs ? import <nixpkgs> {} 
}:
  with pkgs;
  let
    ociTools = callPackage ./ociTools.nix {};
  in
    stdenvNoCC.mkDerivation {
      name = "ociTools-demo";
      buildInputs = [ ociTools.oci-image-tool jq runc ];
    }
