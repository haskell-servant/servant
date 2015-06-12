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
DRY_RUN=false
POSITION="none"
SOURCES_TXT="$( dirname $DIR)/sources.txt"
CABAL=${CABAL:-cabal}
TRAVIS=${TRAVIS:-false}

declare -a SOURCES
readarray -t SOURCES < "$SOURCES_TXT"

join () { local IFS="$1"; shift; echo "$*"; }

versions_equal () {
    local NUM=$(find . -name 'servant*.cabal' | xargs grep "^version:" | awk '{ print $2 }' | uniq -c | wc -l)
    if [ 1 -eq $NUM ] ; then
        return 0
    else
        echo "versions of packages are not all the same!" && exit 1
    fi
}

travis_retry() {
  # From
  # https://github.com/travis-ci/travis-build/blob/18bd04e965b9bfaa49cd6bdcd8dcb1513b8d2fcd/lib/travis/build/templates/header.sh
  local result=0
  local count=1
  while [ $count -le 3 ]; do
    [ $result -ne 0 ] && {
      echo -e "\n${ANSI_RED}The command \"$@\" failed. Retrying, $count of 3.${ANSI_RESET}\n" >&2
    }
    "$@"
    result=$?
    [ $result -eq 0 ] && break
    count=$(($count + 1))
    sleep 1
  done

  [ $count -gt 3 ] && {
    echo -e "\n${ANSI_RED}The command \"$@\" failed 3 times.${ANSI_RESET}\n" >&2
  }

  return $result
}
