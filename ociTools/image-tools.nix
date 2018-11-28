{
  pkgs ? import <nixpkgs> {}
}:
  with pkgs;
  buildGoPackage rec {
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
  }

