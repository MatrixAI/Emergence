with import <nixpkgs> {};
let 
  artifactTools = callPackage ./. {};
in 
{
  nix-hello = artifactTools.buildNixArtifact {
    name = "hello";
    contents = [ hello ];
  };

  oci-hello = artifactTools.pullOCIArtifact {
    imageName = "hello-world";
    imageDigest = "sha256:0add3ace90ecb4adbf7777e9aacf18357296e799f81cabc9fde470971e499788";
    sha256 = "0pnhzsgmm23knzfp6jadpq5yyrhxz32y5nnlj1q3ryqzydg16f9d";
  };

  nix-bash = artifactTools.buildNixArtifact {
    name = "bash";
    contents = [ bash tree ];
    entrypoint = [ "/bin/bash" ];
    terminal = true;
  };

  oci-busybox = artifactTools.pullOCIArtifact {
    imageName = "busybox";
    imageDigest = "sha256:2a03a6059f21e150ae84b0973863609494aad70f0a80eaeb64bddd8d92465812";
    sha256 = "0cl2rhchj3j7h2bha5bv7crh3xv8zfx5a2dfcccv0m61kpzxadwh";
  };

  nix-jupyterhub = artifactTools.buildNixArtifact {
    name = "jupyterhub";
    contents = [ python36Packages.jupyterhub bash coreutils ];
    entrypoint = [ "/bin/bash" ];
    terminal = true;
  };

  nix-jupyter = artifactTools.buildNixArtifact {
    name = "jupyter";
    contents = [ python36Packages.jupyter bash coreutils ];
    entrypoint = [ "/bin/bash" ];
    terminal = true;
  };
}

  
