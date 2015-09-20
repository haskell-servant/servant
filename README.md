# servant - A Type-Level Web DSL

![servant](https://raw.githubusercontent.com/haskell-servant/servant/master/servant.png)

## Getting Started

We have a [tutorial](http://haskell-servant.github.io/tutorial) that
introduces the core features of servant. After this article, you should be able
to write your first servant webservices, learning the rest from the haddocks'
examples.

Other blog posts, videos and slides can be found on the
[website](http://haskell-servant.github.io/).

If you need help, drop by the IRC channel (#servant on freenode) or [mailing
list](https://groups.google.com/forum/#!forum/haskell-servant).

## Contributing

Contributions are very welcome! To hack on the github version, clone the
repository. You can use `cabal`:

```shell
./scripts/start-sandbox.sh # Initialize the sandbox and add-source the packages
./scripts/test-all.sh      # Run all the tests
```

`stack`:

```shell
stack build    # Install and build packages
stack test     # Run all the tests
```

Or `nix`:
```shell
./scripts/generate-nix-files.sh   # Get up-to-date shell.nix files
```

Though we aren't sticklers for style, the `.stylish-haskell.yaml` and `HLint.hs`
files in the repository provide a good baseline for consistency.

Please include a description of the changes in your PR in the `CHANGELOG.md` of
the packages you've changed. And of course, write tests!

