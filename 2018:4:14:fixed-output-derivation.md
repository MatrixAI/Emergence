# Fixed Output Derivation
Using content addressability to ensure that a downstream is as expected.

When building a Nix package, there is a `derivation` function which contains a set of arguments and produces a content-address which points to a `.drv` file with specification of "how to build the derivation".
