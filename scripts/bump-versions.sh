#!/usr/bin/env bash
#===============================================================================
#
#          FILE: bump-versions.sh
#
#         USAGE: ./bump-versions.sh
#
#   DESCRIPTION: Bump the versions for all servant packages
#
#       OPTIONS: See usage
#  REQUIREMENTS: bumper, bash >= 4
#       CREATED: 11.05.2015 21:36
#      REVISION:  ---
#===============================================================================

set -o nounset
set -o errexit

DIR=$( dirname $( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd ))
. "${DIR}/scripts/lib/common.sh"

usage () {
    echo " bump-versions.sh <POSITION> [-d|--dry-run]"
    echo "             | [-h|--help]"
    echo "    Bumps the specified positional version of all servant packages."
    echo "    POSITION is a number between 0 and 3, inclusive."
    exit 0
}

if [ $# -eq 0 ] ; then
    echo "expecting one or more arguments. Got 0"
    usage
elif [ $# -gt 2 ] ; then
    echo "expecting one or more arguments"
    usage
fi

versions_equal

while [ "${1:-unset}" != "unset" ] ; do
    case "$1" in
        -h | --help) usage ;;
        -d | --dry-run) DRY_RUN=true
           shift ;;
        0) if POSITION="none" ; then POSITION=0 ; else usage ; fi
           shift ;;
        1) if POSITION="none" ; then POSITION=1 ; else usage ; fi
           shift ;;
        2) if POSITION="none" ; then POSITION=2 ; else usage ; fi
           shift ;;
        3) if POSITION="none" ; then POSITION=3 ; else usage ; fi
           shift ;;
        *) usage ;;
   esac
done

if $DRY_RUN ; then
   echo "Would have bumped position ${POSITION} on these packages:"
   ( cd "$ROOT" && bumper --dry-run -"$POSITION" $(join , "${SOURCES[@]}") )
else
   ( cd "$ROOT" && bumper -"$POSITION" $(join , "${SOURCES[@]}") )
fi

# Trailing newline, bumper does not ship with its own.
echo
