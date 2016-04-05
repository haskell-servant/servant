# Contributing Guidelines

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


## General

Some things we like:

- Explicit imports
- Upper and lower bounds for packages
- Few dependencies
- -Werror-compatible (for both 7.8 and 7.10)

Though we aren't sticklers for style, the `.stylish-haskell.yaml` and `HLint.hs`
files in the repository provide a good baseline for consistency.

Please include a description of the changes in your PR in the `CHANGELOG.md` of
the packages you've changed. And of course, write tests!

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
issue to discuss it first! (You could release your combinator as a separate
package, of course.)


## New classes

The main benefit of having a new class and package in the main servant repo is
that we get to see via CI whether changes to other packages break the build.
Open an issue to discuss whether a package should be added to the main repo. If
we decide that it can, you can still keep maintainership over it.

Whether or not you want your package to be in the repo, create an issue with
the `news` label if you make a new package so we can know about it!

## Release policy

We are currently moving to a more aggresive release policy, so that you can get
what you contribute from Hackage fairly soon. However, note that prior to major
releases it may take some time in between releases.

## Reporting security issues

Please email haskell-servant-maintainers AT googlegroups DOT com. This group is
private, and accessible only to known maintainers. We will then discuss how to
proceed. Please do not make the issue public before we inform you that we have
a patch ready.
