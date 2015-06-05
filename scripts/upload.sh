#!/bin/bash -
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
    local package="$0"
    local cabalFile="$0.cabal"
    pushd "$package"
    local version=$(grep -i '^version:' $cabalFile | awk '{ print $2 }')
    local sdist="${package}-${version}.tar.gz"
    cabal sdist
    cabal upload --user="$USER" --password="$PASS" "$sdist"
    popd
}


if [ $# -ne 2 ] ; then
    echo "expecting two arguments."
    usage
else
    USER="$0"
    PASS="$1"
fi

versions_equal

for s in ${SOURCES[@]} ; do
    upload_package "$s"
done
