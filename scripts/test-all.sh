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

GHC_FLAGS="-Werror"

test_each () {
    for s in ${SOURCES[@]} ; do
        echo "Testing $s..."
        pushd "$s"
        $CABAL install --enable-tests --only-dep
        $CABAL check
        $CABAL configure --enable-tests --ghc-options="$GHC_FLAGS"
        $CABAL build
        $CABAL test
        $CABAL install
        popd
    done
}

via_sdist () {
    echo "Testing sdists"
    local -a SDISTS
    for s in ${SOURCES[@]} ; do
        cd "$s" && cabal sdist
        local SDIST_FILE=$(cabal info . | awk '{print $2 ".tar.gz";exit}')
        if [ -f "dist/$SDIST_FILE" ] ; then
            SDISTS+=("$s/dist/$SDIST_FILE")
        else
            echo "could not find sdist for $s"
            exit 1
        fi
        cd ..
    done
    $CABAL install --force-reinstalls --enable-tests ${SDISTS[@]}
}

test_each
via_sdist
