with import <nixpkgs> {};
let 
  ociTools = import ./ociTools.nix { inherit pkgs; };
in 
  ociTools.buildSingleLayerImage {
    name = "hello-demo";
    contents = [ hello ];
    compress = true;
  }
  
