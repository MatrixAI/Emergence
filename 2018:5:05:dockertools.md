Today's goal is to understand how dockerTools work.

First let's look at dockerTools build dependencies (inputs to default.nix)

```
  callPackage,
  coreutils,
  docker,
  e2fsprogs,
  findutils,
  go,
  jshon,
  jq,
  lib,
  pkgs,
  pigz,
  nixUnstable,
  perl,
  runCommand,
  rsync,
  shadow,
  stdenv,
  storeDir ? builtins.storeDir,
  utillinux,
  vmTools,
  writeReferencesToFile,
  writeScript,
  writeText
```

