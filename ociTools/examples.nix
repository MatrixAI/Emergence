with import <nixpkgs> {};
let 
  ociTools = callPackage ./ociTools.nix {};
in 
rec {
  hello-static = (hello.override { 
      stdenv = makeStaticBinaries stdenv; 
    }).overrideAttrs (oldAttrs: rec {
      buildInputs = [ glibc.static ];
    });

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
    name = "hello-bundle";
    image = hello-image;
    entrypoint = ["/bin/hello"];
    cwd = "/";
  };

  layered-hello-bundle = ociTools.buildRuntimeBundle {
    name = "layered-hello-bundle";
    image = layered-hello-image;
    entrypoint = ["/bin/hello"];
    cwd = "/";
  };

  get-ref = ociTools.writeImageReferencesToFile {
    name = "get-ref";
    contents = [ hello coreutils ];
  };
}

  
