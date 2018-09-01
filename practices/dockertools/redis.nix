{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
	entrypoint = writeScript "entrypoint.sh" ''
		#!${stdenv.shell}
		set -e
    		exec "$@"
	'';
in
dockerTools.buildImage {
	name = "redis";
	tag = "latest";

	fromImage = null;
	fromImageName = null;
	fromImageTag = "latest";

	contents = [ redis ];
	runAsRoot = ''
		#!${stdenv.shell}
		mkdir -p /data
	'';

	config = {
		Cmd = [ "redis-server" ];
		Entrypoint = [ entrypoint ];
		WorkingDir = "/data";
		Volumes = {
			"/data" = {};
		};
	};
}
