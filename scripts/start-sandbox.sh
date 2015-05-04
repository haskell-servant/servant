#!/bin/bash -
#===============================================================================
#
#          FILE: start-sandbox.sh
#
#         USAGE: ./start-sandbox.sh
#
#   DESCRIPTION: Create sandbox at top-level and add all packages as add-source
#
#  REQUIREMENTS: bash >= 4
#===============================================================================

set -o nounset
set -o errexit

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
SOURCES_TXT="$( dirname $DIR)/sources.txt"
CABAL=${CABAL:-cabal}

declare -a SOURCES
readarray -t SOURCES < "$SOURCES_TXT"

prepare_sandbox () {
    $CABAL sandbox init
    for s in ${SOURCES[@]} ; do
        (cd "$s" && $CABAL sandbox init --sandbox=../.cabal-sandbox && $CABAL sandbox add-source .)
    done
}

prepare_sandbox
