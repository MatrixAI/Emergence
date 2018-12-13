with import <nixpkgs> {};
let 
  artefactTools = callPackage ./. {};
in 
rec {
  nix-bash = artefactTools.buildNixArtefact {
    name = "bash";
    contents = [ bash coreutils ];
    entrypoint = [ "/bin/bash" ];
    terminal = true;
  };

  oci-busybox = artefactTools.pullOCIArtefact {
    imageName = "busybox";
    imageDigest = "sha256:2a03a6059f21e150ae84b0973863609494aad70f0a80eaeb64bddd8d92465812";
    sha256 = "07q380cdki3v1h4dgwyvjai1y4kpcplgw5x611qi373br8pwafv1";
  };
}

  
