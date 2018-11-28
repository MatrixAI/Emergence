with import <nixpkgs> {};
let 
  ociTools = import ./ociTools.nix { inherit pkgs; };
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
  
