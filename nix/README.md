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
-   `ghc865Binary`
-   `ghc884`
-   `ghc8104` - default
-   `ghc901`

### Cabal users

GHC version can be chosen via the nix-shell parameter

`cabal build all`

### Stack version

Since the ghc version is set by the LTS version, it is preferable to use the `ghc8104` version parameter for the nix-shell.

`stack --no-nix --system-ghc <command>`