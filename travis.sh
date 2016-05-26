#!/usr/bin/env bash

set -o errexit

if [ -n "$STACK_YAML" ]; then
  stack test --dry-run
fi

for package in $(cat sources.txt) doc/tutorial ; do
  echo testing $package
  pushd $package
  tinc
  cabal configure --enable-tests --disable-optimization --ghc-options='-Werror'
  cabal build
  cabal test
  popd
done
