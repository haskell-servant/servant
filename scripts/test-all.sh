#!/bin/bash -
#===============================================================================
#
#          FILE: test-all.sh
#
#         USAGE: ./test-all.sh
#
#   DESCRIPTION: Run tests for all source directories listed in $SOURCES.
#                Uses local versions of those sources.
#
#  REQUIREMENTS: bash >= 4
#===============================================================================

set -o nounset
set -o errexit

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
. "$DIR"/lib/common.sh

# TODO: Remove missing-methods after https://github.com/bos/aeson/issues/290
GHC_FLAGS="-Werror -fno-warn-missing-methods"

prepare_sandbox () {
    $CABAL sandbox init
    for s in ${SOURCES[@]} ; do
        (cd "$s" && $CABAL sandbox init --sandbox=../.cabal-sandbox/ && $CABAL sandbox add-source .)
    done
    $CABAL install --enable-tests ${SOURCES[@]}
}

test_each () {
    for s in ${SOURCES[@]} ; do
        echo "Testing $s..."
        pushd "$s"
        $CABAL configure --enable-tests --ghc-options="$GHC_FLAGS"
        $CABAL build
        $CABAL test
        popd
    done
}

prepare_sandbox
test_each
