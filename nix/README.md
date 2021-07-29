You can use the `shell.nix` from this directory
to build the servant packages or even the tutorial
or cookbook if you want to, optionally.

Just the servant packages:

``` sh
$ nix-shell nix/shell.nix
```

Everything needed for the tutorial and the
cookbook too:

``` sh
$ nix-shell nix/shell.nix --arg tutorial true
```

The `shell.nix` file also supports specifying
a particular ghc version, e.g:

``` sh
$ nix-shell nix/shell.nix --argstr compiler ghcHEAD
```

**Possible GHC versions**
-   `ghc822Binary`
-   `ghc865`
-   `ghc884`
-   `ghc8102` - default
