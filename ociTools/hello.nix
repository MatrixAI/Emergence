with import <nixpkgs> {};
let 
  ociTools = import ./ociTools.nix { inherit moreutils  gzip jq nix rsync runCommand writeText; };
in 
  ociTools.buildSingleLayerImage {
    name = "hello-demo";
    contents = [ hello ];
    compress = true;
  }
  
