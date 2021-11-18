#!/usr/bin/env bash
#
# cabal v2-test does not work with GHCJS
# See: https://github.com/haskell/cabal/issues/6175
#
# This invokes cabal-plan to figure out test binaries, and invokes them with node.

cabal-plan list-bins '*:test:*' | while read -r line
do
    testpkg=$(echo "$line" | perl -pe 's/:.*//')
    testexe=$(echo "$line" | awk '{ print $2 }')
    echo "testing $textexe in package $textpkg"
    (cd "$testpkg" && node "$testexe".jsexe/all.js)
done
