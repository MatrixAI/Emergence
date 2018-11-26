{
  pkgs ? import ./pkgs.nix
}:
  with pkgs;
  let 
    drv = (import ./default.nix { inherit pkgs; });
  in 
    drv.overrideAttrs (attrs: {
      src = null;
      buildInputs = attrs.buildInputs ++ [ pkgs.vscodeCpp ];
      shellHook = ''
        echo 'Entering ${attrs.name}'
      '';
    })