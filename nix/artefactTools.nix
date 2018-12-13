{
  pkgs,
  lib,
  runCommand,
}:
{  
  pullOCIArtefact = { 
    imageName,
    imageDigest,
    sha256,
    os ? "linux",
    arch ? "amd64",
    name ? imageName,
    tag ? null
  }:
  let
    outName = name: tag: builtins.replaceStrings ["/" ":"] ["-" "-"] "oci-artefact-${name}${if tag == null then "" else "-" + tag}";
  in
    runCommand (outName name tag) {
      inherit imageName imageDigest;
      impureEnvVars = pkgs.stdenv.lib.fetchers.proxyImpureEnvVars;
      outputHashMode = "recursive";
      outputHashAlgo = "sha256";
      outputHash = sha256;

      nativeBuildInputs = lib.singleton (pkgs.skopeo);
      SSL_CERT_FILE = "${pkgs.cacert.out}/etc/ssl/certs/ca-bundle.crt";

      sourceURL = "docker://${imageName}@${imageDigest}";
    } ''
      skopeo --override-os ${os} --override-arch ${arch} copy "$sourceURL" "oci:$out:${builtins.toString tag}"
    '';

  buildNixArtefact = {
    # Name of the layer
    name,
    # Packages to add to the layer
    contents,
    cwd ? "/",
    entrypoint ? ["sh"],
    terminal ? false, 
  }:
    runCommand "nix-artefact-${name}" { 
      inherit contents cwd;
      entrypoint = builtins.toJSON entrypoint;
      terminal = builtins.toJSON terminal;
      nativeBuildInputs = [ pkgs.jq pkgs.runc ];
    }
    ''
    mkdir $out
    mkdir $out/rootfs

    if [[ -n "$contents" ]]; then
      echo "Adding contents..."
      for item in $contents; do
        echo "Linking $item"
        find $item -mindepth 1 -type d | cut -d/ -f5- | while read dir; do mkdir -p "$out/rootfs/$dir"; done
        find $item -not -type d | cut -d/ -f5- | while read file; do ln -s "$item/$file" "$out/rootfs/$file"; done
      done
    else
      echo "No contents to add to layer."
    fi

    echo "Generating runtime configuration"
    cd $out
    runc spec
    
    specJson=$(cat config.json | jq -r --argjson term $terminal --arg cwd $cwd --argjson args "$entrypoint" '.process.terminal |= $term | .process.cwd |= $cwd | .process.args |= $args')
    echo $specJson > config.json

    echo "Finished building nix artefact '${name}'"
    '';

  # buildClosure = artefact: writeReferencesToFile
  # ''

  #   deps=$(cat $imageClosure)
  #   if [[ -n "$deps" ]]; then
  #     echo "Adding runtime dependencies..."
  #     mkdir layer/nix
  #     mkdir layer/nix/store
  #     for dep in $deps; do
  #       echo "Adding $dep"
  #       rsync -a${if keepContentsDirlinks then "K" else "k"} --chown=${toString uid}:${toString gid} $dep/ layer/$dep
  #     done
  #   fi

  # ''
  # ;
}