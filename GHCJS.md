# Support for ghcjs

This branch of servant implements experimental support for
[ghcjs](https://github.com/ghcjs/ghcjs) for `servant-client`.
This means it allows you to write Haskell code in terms of `servant-client`,
compile it with `ghcjs` to javascript and then use it to execute XHRs from a
browser. The idea is that this should work transparently for the programmer,
i.e. from a user's perspective it should work exactly as it does when compiled
with `ghc`.

## Status

This branch is experimental.

Known caveats:

- Sending bodies in requests doesn't work when using methods `GET` and `HEAD`.
  At least when running the test-suite with node. `GET` and `HEAD` requests are
  not supposed to have request bodies and [xhr2](https://www.npmjs.com/package/xhr2)
	-- which we use to issue XHRs from node -- discards the request bodies for `GET` and
	`HEAD` requests. (This might actually work in some browsers, no clue.) This causes one
  failing test in the test-suite.
- We don't have CI for running the test-suite with `ghcjs`. We tried to make
	that work, but failed miserably. That's the main reason why this is not merged
  to `master`.
- `servant-client` uses libraries that are not optimized for `ghcjs`. I haven't
  investigated this much, but I could imagine that e.g. a native javascript JSON
  parser would be much faster than `aeson` compiled by `ghcjs`.

## Getting it to work

The `stack` file that is used to run the test-suite with `ghcjs` may provide
some inspiration: `servant-client/test/ghcjs/stack-ghcjs.yaml`.

## Running the tests

You can run the tests by doing:

``` bash
./servant-client/test/ghcjs/run-tests.sh
```

## Further development

I propose to use the branch `client-ghcjs` as a place for further development
on `ghcjs` support for `servant-client`. We could

- create PRs against the branch,
- for releases on `master` merge `master` into this branch to not fall behind,
- maybe even create tags, e.g. `ghcjs-v0.6` and `ghcjs-v0.7.1` to give people
  fixed commits to stick to.
