with import <nixpkgs> {};
let 
  ociTools = callPackage ./ociTools.nix {};
  artefactTools = callPackage ./artefactTools.nix {};
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

  docker-test = dockerTools.exportImage {
    fromImage = dockerTools.pullImage {
      imageName = "busybox";
      imageDigest = "sha256:2a03a6059f21e150ae84b0973863609494aad70f0a80eaeb64bddd8d92465812";
      sha256 = "0cjhhgl58kvg6fvbn0ckiv819g03m11r3mldscjzs5v57ammn2bk";
    };
  };

  busybox-docker = dockerTools.pullImage { 
    imageName = "busybox"; 
    imageDigest = "sha256:2a03a6059f21e150ae84b0973863609494aad70f0a80eaeb64bddd8d92465812"; 
    sha256 = "0kqyfd82ab6jin4l6j0izvj4s9dhif96p6f2s6hkb8izdqs321gn"; 
  };

  busybox-oci = artefactTools.pullOCIArtefact {
    imageName = "busybox";
    imageDigest = "sha256:2a03a6059f21e150ae84b0973863609494aad70f0a80eaeb64bddd8d92465812";
    sha256 = "07q380cdki3v1h4dgwyvjai1y4kpcplgw5x611qi373br8pwafv1";
  };

  bash-docker = dockerTools.buildImage {
    name = "bash";
    contents = [ bash ];
    fromImage = busybox-docker;
  };

  bash-artefact = artefactTools.buildNixArtefact {
    name = "bash-artefact";
    contents = [ bash coreutils ];
    entrypoint = [ "/bin/bash" ];
    terminal = true;
  };
}

  
