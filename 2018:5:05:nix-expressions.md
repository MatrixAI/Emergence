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

Nix does not build the derivation unless you tell it to do so, evaluating `derivation` simply creates a `.drv` file.

# TODO:
continue reading http://lethalman.blogspot.com.au/2014/07/nix-pill-6-our-first-derivation.html
