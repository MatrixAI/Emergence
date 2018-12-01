with import <nixpkgs> {};
let 
  ociTools = callPackage ./ociTools.nix {};
in 
rec {
  hello-image = ociTools.buildSingleLayerImage {
    name = "hello-image";
    contents = [ hello ];
    compress = true;
  };

  layered-hello-image = ociTools.buildLayeredImage {
    name = "layered-hello-image";
    layers = [ 
      hello-image
      (ociTools.buildSingleLayerImage {
        name = "moreutils-image";
        contents = [ moreutils ];
        compress = false;
      })
    ];
  };

  hello-bundle = ociTools.buildRuntimeBundle {
    name = "layered-hello-bundle";
    image = layered-hello-image;
    entrypoint = "hello";
    cwd = "/bin/";
  };
}

  
