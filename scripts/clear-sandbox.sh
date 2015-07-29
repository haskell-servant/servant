#!/bin/bash -
#===============================================================================
#
#          FILE: clear-sandbox.sh
#
#         USAGE: ./clear-sandbox.sh
#
#   DESCRIPTION: Clear sandbox at top-level and at all packages
#
#  REQUIREMENTS: bash >= 4
#===============================================================================

set -o nounset
set -o errexit

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
. "$DIR"/lib/common.sh

clear_sandbox () {
    rm -rf .cabal-sandbox cabal.sandbox.config
    for s in ${SOURCES[@]} ; do
        (cd "$s" && rm -rf cabal.sandbox.config dist)
    done
}

clear_sandbox
