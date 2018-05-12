drv="/nix/store/y4h73bmrc9ii5bxg6i7ck6hsf5gqv8ck-foo.drv"
name="foo.drv"

function makePath() {
	type=$1
	descr=$2
	name=$3
	nix_store="/nix/store"
	
	echo -n "$type:$descr:$nix_store:$name" > foo.str
	echo $(nix-hash --type sha256 --truncate --base32 --flat foo.str)
}

h=$(nix-hash --type sha256  $drv)
type="text:/nix/store/xv2iccirbrvklck36f1g7vldn5v58vck-myfile"

makePath $type $h $name
