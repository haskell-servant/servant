#!/bin/bash -
#===============================================================================
#
#          FILE: lib/common.sh
#
#   DESCRIPTION: Common functions for servant's shell scripts
#                Meant to be sourced rather than run.
#
#  REQUIREMENTS: bash >= 4
#===============================================================================


DIR=$( dirname $( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd ))
ROOT=$( dirname $DIR )
DRY_RUN=false
POSITION="none"
SOURCES_TXT="$( dirname $DIR)/sources.txt"
CABAL=${CABAL:-cabal}

declare -a SOURCES
readarray -t SOURCES < "$SOURCES_TXT"

join () { local IFS="$1"; shift; echo "$*"; }

versions_equal () {
    local NUM=$(cd "$ROOT" && find . -name 'servant*.cabal' | xargs grep "^version:" | awk '{ print $2 }' | uniq -c | wc -l)
    if [ 1 -eq $NUM ] ; then
        return 0
    else
        echo "versions of packages are not all the same!" && exit 1
    fi
}
