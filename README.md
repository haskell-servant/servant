# servant - A Type-Level Web DSL

![servant](https://raw.githubusercontent.com/haskell-servant/servant/master/servant.png)

## Getting Started

We have a [tutorial](http://docs.servant.dev/en/stable/tutorial/index.html) that
introduces the core features of servant. After this article, you should be able
to write your first servant webservices, learning the rest from the haddocks'
examples.

The core documentation can be found [here](http://docs.servant.dev/).
Other blog posts, videos and slides can be found on the
[website](http://www.servant.dev/).

If you need help, drop by the IRC channel (#haskell-servant on libera.chat) or [mailing
list](https://groups.google.com/forum/#!forum/haskell-servant).

## Contributing

See `CONTRIBUTING.md`

## Release process outline (by phadej)

- Update changelog and bump versions in `master`
    - `git log --oneline v0.12.. | grep 'Merge pull request'` is a good starting point (use correct previous release tag)
- Create a release branch, e.g. `release-0.13`
    - Release branch is useful for backporting fixes from `master`
- Smoke test in [`servant-universe`](https://github.com/phadej/servant-universe)
    - `git submodule foreach git checkout master` and `git submodule foreach git pull` to get newest of everything.
    - `cabal new-build --enable-tests all` to verify that everything builds, and `cabal new-test all` to run tests
        - It's a good idea to separate these steps, as tests often pass, if they compile :)
    - See `cabal.project` to selectively `allow-newer`
    - If some packages are broken, on your discretisation there are two options:
        - Fix them and make PRs: it's a good idea to test against older `servant` version too.
        - Temporarily comment out broken package
    - If you make a commit for `servant-universe`, you can use it as submodule in private projects to test even more
- When ripples are cleared out:
    - `git tag -s` the release
    - `git push --tags`
    - `cabal sdist` and `cabal upload`

## TechEmpower framework benchmarks

We develop and maintain the servant TFB entry in https://github.com/haskell-servant/FrameworkBenchmarks/

To verify (i.e. compile and test that it works)

```sh
./tfb --mode verify --test servant servant-beam servant-psql-simple --type json plaintext db fortune
```

To compare with  `warp`

```sh
./tfb --mode benchmark --test warp servant servant-beam servant-psql-simple --type json plaintext db fortune
```

To compare with `reitit` (Clojure framework)

```sh
./tfb --mode benchmark --test reitit reitit-async reitit-jdbc servant servant-beam servant-psql-simple --type json plaintext db fortune
```

You can see the visualised results at https://www.techempower.com/benchmarks/#section=test

## Nix

A developer shell.nix file is provided in the `nix` directory

See  [nix/README.md](nix/README.md)
