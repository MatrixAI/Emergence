{
  pkgs ? import <nixpkgs> {}
}:
with pkgs;
rec {
  baseLayout = builtins.toJSON {
    imageLayoutVersion = "1.0.0";
  };

  baseIndex = builtins.toJSON {
    schemaVersion = 2;
    manifests = [];
  };

  baseManifest = builtins.toJSON {
    schemaVersion = 2;
    config = {
      mediaType = "application/vnd.oci.image.config.v1+json";
      size = 0;
      digest = "";
    };
    layers = [];
  };

  baseConfig = builtins.toJSON {
    architecture = "amd64";
    os = "linux";
    rootfs = {
      type = "layers";
      diff_ids = [];
    };
  };

  # Creates a single layer image
  buildSingleLayerImage = {
    # Name of the layer
    name,
    # Files to add to the layer
    contents ? [],
    # Compress with gzip
    compress ? false,
    # When copying the contents into the image, preserve symlinks to
    # directories (see `rsync -K`).  Otherwise, transform those symlinks
    # into directories.
    keepContentsDirlinks ? false
  }:
    let 
      uid = 0;
      gid = 0;
    in
    runCommand "oci-image-${name}" {
      inherit contents compress;
      buildInputs = [ rsync nix gzip jq ];
    }
    ''
    mkdir layer 
    if [[ -n "$contents" ]]; then
      echo "Adding contents..."
      for item in $contents; do
        echo "Adding $item"
        rsync -a${if keepContentsDirlinks then "K" else "k"} --chown=0:0 $item/ layer/
      done
    else
      echo "No contents to add to layer."
    fi

    chmod ug+w layer

    if [[ -n $extraCommands ]]; then
      (cd layer; eval "$extraCommands")
    fi

    # Dir structure
    mkdir $out
    mkdir $out/blobs
    mkdir $out/blobs/sha256

    # Tar up the layer and throw it into 'layer.tar'.
    echo "Packing layer..."
    tar -C layer --hard-dereference --sort=name --mtime="@$SOURCE_DATE_EPOCH" --owner=${toString uid} --group=${toString gid} -cf $out/layer.tar .

    # Compute a checksum of the tarball.
    echo "Computing layer checksum..."
    DIFF_ID=$(nix-hash --type sha256 --flat $out/layer.tar)

    # Add layer to config json
    echo '${baseConfig}' | jq --arg DIFF_ID "sha256:$DIFF_ID" '.rootfs.diff_ids += [$DIFF_ID]' > $out/config.json

    # Checksum and rename the config file
    CONF_DIGEST=$(nix-hash --type sha256 --flat $out/config.json)
    mv $out/config.json $out/blobs/sha256/$CONF_DIGEST

    # Compress if requested
    if [ $compress ]
    then
      echo "Compressing layer tarball..."
      gzip -n $out/layer.tar

      # Compute a checksum for compressed archive.
      echo "Computing compressed layer checksum..."
      DIGEST=$(nix-hash --type sha256 --flat $out/layer.tar.gz)

      # Rename to digest
      mv $out/layer.tar.gz $out/blobs/sha256/$DIGEST

    else
      # Digest will be same as diffId if no compression
      DIGEST=$DIFF_ID

      # Rename to digest
      mv $out/layer.tar $out/blobs/sha256/$DIGEST
    fi

    # Add layer and config to manifest
    SIZE=$(du -b $out/blobs/sha256/$DIGEST | cut -f1)
    CONF_SIZE=$(du -b $out/blobs/sha256/$CONF_DIGEST | cut -f1)
    if [ $compress ]; then
      MEDIA_TYPE="application/vnd.oci.image.layer.v1.tar+gzip"
    else
      MEDIA_TYPE="application/vnd.oci.image.layer.v1.tar"
    fi 
    echo '${baseManifest}' | jq --arg CONF_DIGEST "sha256:$CONF_DIGEST" --argjson CONF_SIZE "$CONF_SIZE" --arg DIGEST "sha256:$DIGEST" --argjson SIZE "$SIZE" --arg MEDIA_TYPE "$MEDIA_TYPE" '
        .config.digest |= $CONF_DIGEST
        | .config.size |= $CONF_SIZE 
        | .layers += [{mediaType: $MEDIA_TYPE, digest: $DIGEST, size: $SIZE}]' > $out/manifest.json

    # Checksum and rename the manifest
    MANIFEST_DIGEST=$(nix-hash --type sha256 --flat $out/manifest.json)
    mv $out/manifest.json $out/blobs/sha256/$MANIFEST_DIGEST
    
    # Add manifest to the index file
    MANIFEST_SIZE=$(du -b $out/blobs/sha256/$MANIFEST_DIGEST | cut -f1)
    echo '${baseIndex}' | jq --arg MANIFEST_DIGEST "sha256:$MANIFEST_DIGEST" --argjson MANIFEST_SIZE "$MANIFEST_SIZE" '
      .manifests += [{
        mediaType: "application/vnd.oci.image.manifest.v1+json",
        size: $MANIFEST_SIZE,
        digest: $MANIFEST_DIGEST,
        platform: {
          architecture: "amd64",
          os: "linux"
        }
      }]' > $out/index.json

    # Create the layout file
    echo '${baseLayout}' > $out/oci-layout

    echo "Finished building single layer image '${name}'"
    '';

  # Applies multiple single layer images to create one image
  buildLayeredImage = {
    name,
    # Prebuilt layers to use
    layers ? [],
    # Compress with gzip
    compress ? false,
    # When copying the contents into the image, preserve symlinks to
    # directories (see `rsync -K`).  Otherwise, transform those symlinks
    # into directories.
    keepContentsDirlinks ? false
  }:
    let 
      uid = 0;
      gid = 0;
    in
    runCommand "oci-image-${name}" {
      inherit layers;
      buildInputs = [ rsync nix gzip jq moreutils ];
    }
    ''
    mkdir image
    if [[ -n "$layers" ]]; then
      # Dir structure
      mkdir $out
      mkdir $out/blobs
      mkdir $out/blobs/sha256

      CONFIG_JSON='${baseConfig}'
      MANIFEST_JSON='${baseManifest}'

      echo "Fetching layers..."
      for item in $layers; do
        # Parse index, manifest and config JSONs
        echo "Parsing JSONs"
        MANIFEST_DIGEST=$(jq -r '.manifests[0].digest' $item/index.json | cut -d: -f2)
        CONFIG_DIGEST=$(jq -r '.config.digest' $item/blobs/sha256/$MANIFEST_DIGEST | cut -d: -f2)
        DIGEST=$(jq -r '.layers[0].digest' $item/blobs/sha256/$MANIFEST_DIGEST | cut -d: -f2)
        LAYER=$(jq -r '.layers[0]' $item/blobs/sha256/$MANIFEST_DIGEST)
        DIFF_ID=$(jq -r '.rootfs.diff_ids[0]' $item/blobs/sha256/$CONFIG_DIGEST | cut -d: -f2)

        # Copy layers
        echo "Copying $item"
        cp -r $item/blobs/sha256/$DIGEST $out/blobs/sha256/

        # Add metadata to JSONs
        echo "Copying metadata"
        CONFIG_JSON=$(echo "$CONFIG_JSON" | jq --arg DIFF_ID "sha256:$DIFF_ID" '.rootfs.diff_ids += [$DIFF_ID]')
        MANIFEST_JSON=$(echo "$MANIFEST_JSON" | jq --argjson LAYER "$LAYER" '.layers += [ $LAYER ]')

      done
    else
      echo "No contents to add to layer."
    fi

    # Write config json to file
    echo "$CONFIG_JSON" > $out/config.json

    # Compute checksum of and rename config file
    CONF_DIGEST=$(nix-hash --type sha256 --flat $out/config.json)
    mv $out/config.json $out/blobs/sha256/$CONF_DIGEST

    # Add config file descriptor to manifest
    CONF_SIZE=$(du -b $out/blobs/sha256/$CONF_DIGEST | cut -f1)
    echo "$MANIFEST_JSON" | jq --arg CONF_DIGEST "sha256:$CONF_DIGEST" --argjson CONF_SIZE "$CONF_SIZE" '
      .config.digest |= $CONF_DIGEST
      | .config.size |= $CONF_SIZE' > $out/manifest.json

    # Checksum and rename the manifest
    MANIFEST_DIGEST=$(nix-hash --type sha256 --flat $out/manifest.json)
    mv $out/manifest.json $out/blobs/sha256/$MANIFEST_DIGEST
    
    # Add manifest to the index file
    MANIFEST_SIZE=$(du -b $out/blobs/sha256/$MANIFEST_DIGEST | cut -f1)
    echo '${baseIndex}' | jq --arg MANIFEST_DIGEST "sha256:$MANIFEST_DIGEST" --argjson MANIFEST_SIZE "$MANIFEST_SIZE" '
      .manifests += [{
        mediaType: "application/vnd.oci.image.manifest.v1+json",
        size: $MANIFEST_SIZE,
        digest: $MANIFEST_DIGEST,
        platform: {
          architecture: "amd64",
          os: "linux"
        }
      }]' > $out/index.json

    # Create the layout file
    echo '${baseLayout}' > $out/oci-layout

    echo "Finished building layered image '${name}'"
    '';

  oci-image-tool = buildGoPackage rec {
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

  buildRuntimeBundle = {
    name,
    image,
    cwd ? "/",
    entrypoint ? ["sh"],
    terminal ? false, 
    # When copying the contents into the image, preserve symlinks to
    # directories (see `rsync -K`).  Otherwise, transform those symlinks
    # into directories.
    keepContentsDirlinks ? false
  }:
    runCommand "oci-bundle-${name}" {
      inherit image cwd;
      entrypoint = builtins.toJSON entrypoint;
      terminal = builtins.toJSON terminal;
      buildInputs = [ jq rsync runc ];
    } 
    ''
    mkdir $out
    
    indexJson=$(cat $image/index.json)

    manifestFn=$(echo "$indexJson" | jq -r '
      .manifests[] | 
      select(.platform == {"architecture": "amd64", "os": "linux"}) | 
      .digest
    ' | cut -d: -f2)
    manifestJson=$(cat $image/blobs/sha256/$manifestFn)
    
    configFn=$(echo "$manifestJson" | jq -r '.config.digest' | cut -d: -f2)
    configJson=$(cat $image/blobs/sha256/$configFn)

    layers=$(echo "$manifestJson" | jq -r '.layers[] | .digest + if .mediaType == "application/vnd.oci.image.layer.v1.tar+gzip" then ":1" else ":0" end')
    
    for layer in $layers; do
      compress=$(echo "$layer" | cut -d: -f3)
      layerFn=$(echo "$layer" | cut -d: -f2)

      mkdir layer-$layerFn

      if [[ 1 == $compress ]]; then
        tar -C layer-$layerFn -xzvf $image/blobs/sha256/$layerFn
      else
        tar -C layer-$layerFn -xvf $image/blobs/sha256/$layerFn      
      fi

      # TODO: if there is a whiteout file, remove original 

      rsync -a${if keepContentsDirlinks then "K" else "k"} --chown=0:0 layer-$layerFn/ $out/rootfs
    done

    # TODO: Properly generate runtime spec
    cd $out
    runc spec
    
    specJson=$(cat config.json | jq -r --argjson term $terminal --arg cwd $cwd --argjson args "$entrypoint" '.process.terminal |= $term | .process.cwd |= $cwd | .process.args |= $args')
    echo $specJson > config.json
    '';
} 
