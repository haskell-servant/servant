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

## Quotes

> Servant may just be the coolest single Haskell library.
>   - <cite>ephrion, [reddit](https://www.reddit.com/r/haskell/comments/3g8jb0/contenttype_bliss/ctw75hg)</cite>

<br>

> servant is awesome so far, there's really nothing else like it right now for
> haskell. ... It's absolutely fantastic, I look back at my old [other haskell
> web framework] code and it might as well have been javascript. Great work,
> just great.
>   - <cite>dmj, #servant</cite>

<br>

> It is one of the coolest libraries I have ever seen.
>   - <cite>Taylor Fausak, [Type safe web services in Haskell with Servant](http://taylor.fausak.me/2015/08/23/type-safe-web-services-in-haskell-with-servant/)</cite>
