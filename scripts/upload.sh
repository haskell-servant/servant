#!/usr/bin/env bash
#===============================================================================
#
#          FILE: upload.sh
#
#         USAGE: ./upload.sh <USER> <PASSWORD>
#
#   DESCRIPTION: Uploads all servant packages to Hackage
#
#  REQUIREMENTS: cabal, bash >= 4
#        AUTHOR: Julian K. Arni
#       CREATED: 05.06.2015 13:05
#===============================================================================

set -o nounset
set -o errexit

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
. "$DIR"/lib/common.sh

usage () {
    echo " upload.sh <USER> <PASSWORD>"
    echo "    Uploads all servant packages to Hackage"
    exit 0
}


upload_package () {
    local package="$1"
    local user="$2"
    local pass="$3"
    local cabalFile="$package.cabal"
    pushd "$package"
    local version=$(grep -i '^version:' $cabalFile | awk '{ print $2 }')
    local sdist="dist/${package}-${version}.tar.gz"
    cabal sdist
    echo "User is: $user"
    cabal upload --user="$user" --password="$pass" "$sdist"
    popd
}


if [ $# -ne 2 ] ; then
    echo "expecting two arguments."
    usage
fi

versions_equal

for s in ${SOURCES[@]} ; do
    upload_package "$s" "$1" "$2"
done
