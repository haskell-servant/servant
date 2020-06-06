# Contributing Guidelines

Contributions are very welcome! To hack on the github version, clone the
repository. You can use `cabal`:

```shell
./scripts/start-sandbox.sh # Initialize the sandbox and add-source the packages
./scripts/test-all.sh      # Run all the tests
```

Or `stack`:

```shell
stack setup                    # Downloads and installs a proper GHC version if necessary
stack build --fast --pedantic  # Install dependencies and build packages
stack test                     # Run all the tests
```

Or `nix`:
```shell
./scripts/generate-nix-files.sh   # Get up-to-date shell.nix files
```

To build the docs, see `doc/README.md`.

## General

Some things we like:

- Explicit imports
- Upper and lower bounds for packages
- Few dependencies
- -Werror-compatible (7.8, 7.10 and 8.0)

Though we aren't sticklers for style, the `.stylish-haskell.yaml` and `HLint.hs`
files in the repository provide a good baseline for consistency.

**Important**: please do not modify the versions of the servant packages you are sending patches for.

## Changelog entries

We experiment with using [changelog-d tool](https://github.com/phadej/changelog-d) to assemble changelogs.
You are not required to install it.

In each PR please add a file to `changelog.d` directory named after issue you are solving or the pull request itself (in a separate commit after you know the pull request number). For example

```cabal
synopsis: One sentence summary of the change.
prs: #1219
issues: #1028

description: {

A longer description. Small changes don't need this.
Bigger ones definitely do, for example we try to include migration hints
for breaking changes.

However if you don't know what to write, that's ok too.

By the way, the braces around are omitted when the file is parsed.
They can be used so the field doesn't need to be indented, which is handy
for prose.

}
```

## PR process

We try to give timely reviews to PRs that pass CI. If CI for your PR fails, we
may close the PR if it has been open for too long (though you should feel free
to reopen when the issues have been fixed).

We require two +1 from the maintainers of the repo. If you feel like there has
not been a timely response to a PR, you can ping the Maintainers group (with
`@haskell-servant/maintainers`).

## New combinators

We encourage people to experiment with new combinators and instances - it is
one of the most powerful ways of using `servant`, and a wonderful way of
getting to know it better. If you do write a new combinator, we would love to
know about it! Either hop on #servant on freenode and let us know, or open an
issue with the `news` tag (which we will close when we read it).

As for adding them to the main repo: maintaining combinators can be expensive,
since official combinators must have instances for all classes (and new classes
come along fairly frequently). We therefore have to be quite selective about
those that we accept. If you're considering writing a new combinator, open an
issue to discuss it first!  Or contribute it to the
[servant-contrib](https://github.com/haskell-servant/servant-contrib) repository.
You could release your combinator as a separate package, of course.


## New classes

The main benefit of having a new class and package in the main servant repo is
that we get to see via CI whether changes to other packages break the build.
Open an issue to discuss whether a package should be added to the main repo. If
we decide that it can, you can still keep maintainership over it.

Whether or not you want your package to be in the repo, create an issue with
the `news` label if you make a new package so we can know about it!

## Release policy

We are currently moving to a more aggressive release policy, so that you can get
what you contribute from Hackage fairly soon. However, note that prior to major
releases it may take some time in between releases.

## Reporting security issues

Please email haskell-servant-maintainers AT googlegroups DOT com. This group is
private, and accessible only to known maintainers. We will then discuss how to
proceed. Please do not make the issue public before we inform you that we have
a patch ready.
