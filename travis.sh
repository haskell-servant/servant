#!/usr/bin/env bash

set -o errexit

for package in $(cat sources.txt) doc/tutorial ; do
  echo testing $package
  pushd $package
  tinc
  cabal configure --enable-tests --disable-optimization
  cabal build
  cabal test
  popd
done
