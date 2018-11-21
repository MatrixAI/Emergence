{
  gzip,
  jq,
  nix,
  rsync,
  runCommand,
  writeText,
}:

rec {
  baseLayout = writeText "oci-layout" (builtins.toJSON {
    imageLayoutVersion = "1.0.0";
  });

  baseIndex = writeText "index.json" (builtins.toJSON {
    schemaVersion = 2;
    manifests = [];
  });

  baseManifest = compress: writeText "manifest.json" (builtins.toJSON {
    schemaVersion = 2;
    config = {
      mediaType = "application/vnd.oci.image.config.v1+json";
      size = 0;
      digest = "";
    };
    layers = [{
      mediaType = if compress 
        then "application/vnd.oci.image.layer.v1.tar+gzip" 
        else "application/vnd.oci.image.layer.v1.tar";
      size = 0;
      digest = "";
    }];
  });

  baseConfig = writeText "config.json" (builtins.toJSON {
    architecture = "amd64";
    os = "linux";
    rootfs = {
      type = "layers";
      diff_ids = [];
    };
  });

  buildSingleLayerImage = {
    # Name of the layer
    name,
    # Files to add to the layer
    contents ? null,
    # Compress with gzip
    compress ? false,
    # When copying the contents into the image, preserve symlinks to
    # directories (see `rsync -K`).  Otherwise, transform those symlinks
    # into directories.
    keepContentsDirlinks ? false,
    # Additional commands to run on the layer before it is tar'd up.
    extraCommands ? "", uid ? 0, gid ? 0
  }:
    runCommand "oci-layer-${name}" {
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
    cat ${baseConfig} | jq --arg DIFF_ID "sha256:$DIFF_ID" '.rootfs.diff_ids += [$DIFF_ID]' > $out/config.json

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
    cat ${baseManifest compress} | jq --arg CONF_DIGEST "sha256:$CONF_DIGEST" --arg CONF_SIZE $CONF_SIZE --arg DIGEST "sha256:$DIGEST" --arg SIZE $SIZE '
        .config.digest |= $CONF_DIGEST
        | .config.size |= $CONF_SIZE 
        | .layers[0].digest |= $DIGEST
        | .layers[0].size |= $SIZE' > $out/manifest.json

    # Checksum and rename the manifest
    MANIFEST_DIGEST=$(nix-hash --type sha256 --flat $out/manifest.json)
    mv $out/manifest.json $out/blobs/sha256/$MANIFEST_DIGEST
    
    # Add manifest to the index file
    MANIFEST_SIZE=$(du -b $out/blobs/sha256/$MANIFEST_DIGEST | cut -f1)
    cat ${baseIndex} | jq --arg MANIFEST_DIGEST "sha256:$MANIFEST_DIGEST" --arg MANIFEST_SIZE $MANIFEST_SIZE '
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
    cp ${baseLayout} $out/oci-layout

    echo "Finished building single layer image '${name}'"
    '';
}
