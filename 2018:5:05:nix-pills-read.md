# Nix Expressions
Nix is strongly typed but not statically typed. That is we cannot mix strings and integers, we must first do the conversion.

Paths are parsed as long as there is a `/`, hence for current path, use `./.`.

Insert whole Nix expressions **inside string** with `"${...}"`, like `"$(...)"` in bash


# Lists
Are separated by space.

```nix
[12 4 true "foo"]
```

# Sets
```nix
{key-a = "abc"; key-b = 123;}
```

# If expression
```
if <cond> then c else d
```
Note that we must have an `else` branch because an expression must have a value in all cases.

# Let expression
Is used to define local variables to inner expressions.

```nix
> let a = "foo"; in a
"foo"
```

This first assign variabels, then `in` expression. The overall result will be the final expression after `in`.

For example: `let a = 3; in let b = 4; in a + b` will give us `7`.

variables in the let expression cannot be referred from the outside. Same applies to recursive sets.

# Recursive Sets
Simply sets with some attributes refers to other attributes within the set.


```
rec {
	a = b;
	b = 3;
}
```

will evaluate to:

```
{a = 3; b = 3;}
```

# With expression

`with` expression is used to include symbols into the scope.

```
with longName; longName.a + longName.b
```

Notice that in some versions of Nix this might work:
```
with longName; a + b
```
But apparently not with Nix 2.0.1 or higher, referencing the *withed* name is necessary.

# Laziness
Nix evaluates expressions when needed.


# Functions
## Nameless and single parameter
```
sq = x : x * x
```

## More than one parameter
```
mul = a : (b : a * b)
```

## Argument set
Argument set allows pattern matching over a set in the parameter.

```
> mul = s : s.a * s.b

> mul {a = 3; b = 4;}
12
```

or more commonly:

```
> mul = {a, b}: a * b

> mul {a = 3; b = 3;}
9
```

## Default and variadic attributes
It is possible to specify default attributes in argument set:

```
> mul = {a, b ? 3}: a * b
```

Or passing more attributes (variadic) than the expected ones:

```
> mul = {a, b, ...}: a * b
```

# Imports

The `import` function is built-in and provides a way to parse a `.nix` file.

If we have a file a.nix
```
# In a.nix
3
```

Then if we do
```
nix-repl> import ,/a.nix
3
```

# The Derivation Function
The `derivation` fucntion is used to create derivations.

The `derivation` is simply a set, with some attributes. Therefore you can pass the derivation around with variables like anything else.

The derivation function receives a set as its first argument, It MUST contain the following attribtues:

- `name`: name of the derivation
- `system`: name of the system which the derivation can be built
- `builder`: the binary program to build the derivation

`builtins.currentSystem` tells us the current system as seen by Nix.

Nix does not build the derivation unless you tell it to do so, evaluating `derivation` simply creates a `.drv` file.We can see the derived hash in the nix store, at this point, the directory is still empty.

The hash of the out path is based solely on the **input derivations** in the current version of Nix, **not in the contents of the build product**.

Store derivations can be computed without verification on builder, platforms and so on, the verification and input content addresses are only recursively computed when the store derivation is *built*. Derivations are not built during the evaluation stage of the nix expression, however when a derivation is parsed, the parser will replace all undeterministic sources with `/nix/store` paths. Note that supplying a string representation of a path will not work properly.

*My findings* `nix-repl` seems to be caching the store derivations that takes in the same input in the same session, even if the *content* of those inputs have changed. Restarting `nix-repl` is needed to compute a different store derivation.

## The builder environment
If we inspect those environment variables printed during the built process.

```bash
building '/nix/store/rng5lg655s95dgman552g5flc4v6rdm5-foo.drv'...
declare -x HOME="/homeless-shelter" # $HOME is not your home directory, /homeless-shelter doesnt exist.
declare -x NIX_BUILD_CORES="0" # nix config options
declare -x NIX_BUILD_TOP="/build"
declare -x NIX_LOG_FD="2"
declare -x NIX_STORE="/nix/store" # nix config options
declare -x OLDPWD
declare -x PATH="/path-not-set" # same as $HOME
declare -x PWD="/build"
declare -x SHLVL="1"
declare -x TEMP="/build"
declare -x TEMPDIR="/build"
declare -x TMP="/build"
declare -x TMPDIR="/build"
declare -x builder="/nix/store/zqh3l3lyw32q1ayb15bnvg9f24j5v2p0-bash-4.4-p12/bin/bash" # in drv
declare -x name="foo" # in drv
declare -x out="/nix/store/a490539pkf2hamnywijkg5d979b4fj54-foo" # in drv
declare -x system="x86_64-linux" # in drv
```

# The // Operator

The `//` operator is an operator between two sets. The result is the union of the two sets. In case of conflicts in attribute names, right set is preferred.

# nix-build

`nix-build` does two jobs:
	- nix-instantiate: parse and evaluate `simple.nix` and return the `.drv` file.
	- nix-store -r: realise the .drv file and build it.
Finally it creates symlink.

# Nix Derivations
To refer to other derivations on disk, use `outPath`.
an `outPath` in a attribute set is much like `toString()` in other languages. `builtins.toString` will take an attribute set and print the content of `outPath`.

## Derivation Build process
1. (with `nix-instantiate`) Nix expressions is parsed, interpreted and finally returns a derivtion set. During evaluation, you can refer to other derivations because Nix will create .drv files and we willl know outPaths beforehand.
2. (With `nix-store -r`) The .drv from the derivations set is built, first building .drv  inputs (build dependencies).

Note `nix-build` is a wrapper for the above two commands.

## Dependencies
To refer to other dependencies, we use the outPath of the drivation. So for example, if we want to use the binaries from coreutil:
```
nix-repl> :l <nixpkgs>
Added 8612 variables.

nix-repl> coreutils
«derivation /nix/store/kp5zb1210ys586k078iz4qmpjflqkrd5-coreutils-8.29.drv»

nix-repl> builtins.toString coreutils
"/nix/store/cb3slv3szhp46xkrczqw7mscy5mnk64l-coreutils-8.29"
```

Now we can refer to binaries like this:
```
nix-repl> "${coreutils}/bin/true"
```

## Generating Store Path

### Input Path
The outPath of a input file is generated by:

1. Compute the SHA256 of the NAR serialization of the file.
	This can be done by:
	```
	$ nix-hash -type sha256 myfile
	2bfef67de873c54551d884fdab3055d84d573e654efa79db3c0d7b98883f9ee3
	```
	or
	```
	$ nix-store --dump myfile | sha256sum
	2bfef67de873c54551d884fdab3055d84d573e654efa79db3c0d7b98883f9ee3
	```
2. build the string description: nix uses a special string which includes the hash, the path type and the filename
	```
	$ echo -n "source:sha256:"2bfef67de873c54551d884fdab3055d84d573e654efa79db3c0d7b98883f9ee3:/nix/store:myfile" > myfile.str
	```
3. Compute the final hash:
	```
	$ nix-hash --type sha256 --truncate --base32 --flat myfile.str
	xv2iccirbrvklck36f1g7vldn5v58vck
	```

### OutPath
Nix knows the outPath before we build the derivation because the outPath only depends on inputs.

It's computed in a similar way to source paths, except that .drv is hashed and the type of derivation is `output:out`. In case of multiple outputs, we may have different `output:<id>`.

1. At the time when outPath is generated, the .drv contains `""` for each out path. Hence to replicate the process we do:
	```
	$ cp -f /nix/store/y4h73bmrc9ii5bxg6i7ck6hsf5gqv8ck-foo.drv myout.drv
	$ sed -i 's,/nix/store/hs0yi5n5nw6micqhy8l1igkbhqdkzqa1-foo,,g' myout.drv
	```
2. Then:
	```
	$ sha256sum myout.drv
	1bdc41b9649a0d59f270a92d69ce6b5af0bc82b46cb9d9441ebc6620665f40b5  myout.drv
	$ echo -n "output:out:sha256:1bdc41b9649a0d59f270a92d69ce6b5af0bc82b46cb9d9441ebc6620665f40b5:/nix/store:foo" > myout.str
	$ nix-hash --type sha256 --truncate --base32 --flat myout.str
	hs0yi5n5nw6micqhy8l1igkbhqdkzqa1
	```

### Fixed-output Derivation Path

This path is used when we know beforehand the integrity hash of a file. This is often used for tarballs.

A derivation can take three special attributes: `outputHashMode`, `outputHash` and `outputHashAlgo`.

say if we have this derivation:

```bash
$ echo mycontent > myfile
$sha256sum myfile
f3f3c4763037e059b4d834eaf68595bbc02ba19f6d2a500dce06d124e2cd99bb  myfile

nix-repl> derivation {
		name = "bar";
		system = "x86_64-linux";
		builder = "none";
		outputHashMode = "flat";
		outputHashAlgo = "sha256";
		outputHash = "f3f3c4763037e059b4d834eaf68595bbc02ba19f6d2a500dce06d124e2cd99bb";
	}
«derivation /nix/store/ymsf5zcqr9wlkkqdjwhqllgwa97rff5i-bar.drv»
```

If we inspect the store derivation we can see that it also includes attributes `hashAlgo` and `hash` in `out`.

It doesn't matter which input derivations are being used, the final out path must only depend on the declared hash. During store derivation generations, nix creates an intermediate string representation of the fixed-output content.

```bash
$ echo -n "fixed:out:sha256:f3f3c4763037e059b4d834eaf68595bbc02ba19f6d2a500dce06d124e2cd99bb:" > mycontent.str

$ sha256sum mycontent.str
423e6fdef56d53251c5939359c375bf21ea07aaa8d89ca5798fb374dbcfd7639  myfile.str
```

Then proceed as if it was a normal derivation output path:

```bash
$ echo -n "output:out:sha256:423e6fdef56d53251c5939359c375bf21ea07aaa8d89ca5798fb374dbcfd7639:/nix/store:bar" > myfile.str
$ nix-hash --type sha256 --truncate --base32 --flat myfile.str
a00d5f71k0vp5a6klkls0mvr1f7sx6ch
```

# Final Store derivation path

// TODO

# Nix Path
the `$NIX_PATH` variable is similar to `$PATH`. it can be used within Nix expressions to search for tools with `<tool>`. For example, `<nixpkgs>`.
