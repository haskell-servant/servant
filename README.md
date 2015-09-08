# servant

[![Build Status](https://secure.travis-ci.org/haskell-servant/servant.svg)](http://travis-ci.org/haskell-servant/servant)
[![Coverage Status](https://coveralls.io/repos/haskell-servant/servant/badge.svg)](https://coveralls.io/r/haskell-servant/servant)

![servant](https://raw.githubusercontent.com/haskell-servant/servant/master/servant.png)

These libraries provides a family of combinators to define webservices and
automatically generate the documentation and client-side querying functions for
each endpoint.

In order to minimize the dependencies depending on your needs, we provide these
features under different packages.

- `servant`, which contains everything you need to *declare* a webservice API.
- `servant-server`, which lets you *implement* an HTTP server with handlers for
each endpoint of an API.
- `servant-client`, which lets you derive automatically Haskell functions that
let you query each endpoint of a `servant` webservice.
- `servant-docs`, which lets you generate API docs for your webservice.
- `servant-js`, which lets you derive Javascript functions (using vanilla JS
ajax requests, angular or jquery) to query your API's endpoints, in the same
spirit as `servant-client`.
- `servant-blaze` and `servant-lucid` provide easy HTML rendering of your data
as an `HTML` content-type "combinator".

## Tutorial

We have a [tutorial](http://haskell-servant.github.io/tutorial) guide that
introduces the core types and features of servant. After this article, you
should be able to write your first servant webservices, learning the rest from
the haddocks' examples.

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
