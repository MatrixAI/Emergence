with import <nixpkgs> {};
let 
  artefactTools = callPackage ./artefactTools.nix {};
in 
rec {
  bash-artefact = artefactTools.buildNixArtefact {
    name = "bash-artefact";
    contents = [ bash coreutils ];
    entrypoint = [ "/bin/bash" ];
    terminal = true;
  };
}

  
