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
GHC_FLAGS="-Werror"
SOURCES_TXT="$( dirname $DIR)/sources.txt"

declare -a SOURCES
readarray -t SOURCES < "$SOURCES_TXT"


prepare_sandbox () {
    cabal sandbox init
    for s in ${SOURCES[@]} ; do
        (cd "$s" && cabal sandbox init --sandbox=../ && cabal sandbox add-source .)
    done
}

test_each () {
    for s in ${SOURCES[@]} ; do
        echo "Testing $s..."
        cd "$s"
        cabal install --only-dependencies --enable-tests
        cabal configure --enable-tests --ghc-options="$GHC_FLAGS"
        cabal build
        cabal test
        cd ..
    done
}

prepare_sandbox
test_each
