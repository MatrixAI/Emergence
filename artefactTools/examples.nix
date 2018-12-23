with import <nixpkgs> {};
let 
  artefactTools = callPackage ./. {};
in 
{
  nix-bash = artefactTools.buildNixArtefact {
    name = "bash";
    contents = [ bash tree ];
    entrypoint = [ "/bin/bash" ];
    terminal = true;
  };

  oci-busybox = artefactTools.pullOCIArtefact {
    imageName = "busybox";
    imageDigest = "sha256:2a03a6059f21e150ae84b0973863609494aad70f0a80eaeb64bddd8d92465812";
    sha256 = "0cl2rhchj3j7h2bha5bv7crh3xv8zfx5a2dfcccv0m61kpzxadwh";
  };

  nix-jupyterhub = artefactTools.buildNixArtefact {
    name = "jupyterhub";
    contents = [ python36Packages.jupyterhub bash coreutils ];
    entrypoint = [ "/bin/bash" ];
    terminal = true;
  };

  nix-jupyter = artefactTools.buildNixArtefact {
    name = "jupyter";
    contents = [ python36Packages.jupyter bash coreutils ];
    entrypoint = [ "/bin/bash" ];
    terminal = true;
  };
}

  
