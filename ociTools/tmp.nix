with import <nixpkgs> {};
let 
  ociTools = import ./ociTools.nix { inherit gzip jq moreutils nix rsync runCommand writeText; };
in 
  ociTools.buildLayeredImage {
    name = "layered-hello-demo";
    layers = [ 
      (ociTools.buildSingleLayerImage {
        name = "hello-demo";
        contents = [ hello ];
        compress = true;
      }) 
      (ociTools.buildSingleLayerImage {
        name = "another";
        contents = [ moreutils ];
        compress = false;
      })
    ];
  }
  