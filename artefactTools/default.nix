{
  pkgs,
  lib,
  runCommand,
  dockerTools,
  fetchFromGitHub,
}:
rec {  
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
    outName = builtins.replaceStrings ["/" ":"] ["-" "-"] "oci-artefact-${name}${if tag == null then "" else "-" + tag}";
    oci-image-tool = pkgs.buildGoPackage rec {
      name = "oci-image-tools-${version}";
      version = "1.0.0-rc1";

      goPackagePath = "github.com/opencontainers/image-tools";
      subPackages = [ "cmd/oci-image-tool" ];

      src = fetchFromGitHub {
        owner = "opencontainers";
        repo = "image-tools";
        rev = "v${version}";
        sha256 = "0c4n69smqlkf0r6khy9gbg5f810qh9g8jqsl9kibb0dyswizr14r";
      };
    };
  in
    runCommand outName {
      impureEnvVars = pkgs.stdenv.lib.fetchers.proxyImpureEnvVars;
      outputHashMode = "recursive";
      outputHashAlgo = "sha256";
      outputHash = sha256;

      nativeBuildInputs = [ pkgs.skopeo oci-image-tool ];
      SSL_CERT_FILE = "${pkgs.cacert.out}/etc/ssl/certs/ca-bundle.crt";

      sourceURL = "docker://${imageName}@${imageDigest}";
    } ''
      mkdir image
      skopeo --override-os ${os} --override-arch ${arch} copy "$sourceURL" "oci:image:${builtins.toString tag}"

      mkdir $out
      mkdir $out/rootfs
      oci-image-tool unpack --ref platform.os=${os} image/ $out/rootfs/
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
}