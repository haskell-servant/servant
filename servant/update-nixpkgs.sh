#!/bin/bash -
#===============================================================================
#
#          FILE: update-nixpkgs.sh
#
#         USAGE: ./update-nixpkgs.sh
#
#   DESCRIPTION: Update servant packages in nixpkgs.
#
#       OPTIONS: -d <NIXPKGS DIR>
#  REQUIREMENTS: cabal2nix, git, hub
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Julian Arni <jkarni@gmail.com>,
#  ORGANIZATION: Zalora SEA
#       CREATED: 09.01.2015 15:38
#      REVISION:  ---
#===============================================================================

set -o nounset
set -e

# Variables
declare -ar -x PKGS=(servant servant-client servant-server servant-jquery servant-docs)
declare -a CHANGED

PKG_NO=${#PKGS[@]}
GIT_DIR=$(mktemp -d)
FORK=""
BRANCH_NAME=servant-changes
HASKELL_LIBS=pkgs/development/libraries/haskell

cleanup () {
    echo "Removing temporary directory $GIT_DIR"
    rm -rf $GIT_DIR
}

while getopts f: OPT ; do
    case $OPT in
        f) FORK=$OPTARG ;;
    esac
done

# Check if we can proceed

if [[ -z "$FORK" ]] ; then
    echo "Your fork must be specified with -f FORK"
    exit 1
fi

cd $GIT_DIR

git clone --depth 1 git@github.com:NixOS/nixpkgs.git && cd nixpkgs

trap cleanup INT TERM EXIT

for ((i = 0 ; i < PKG_NO ; i++ )); do
    PKG=${PKGS[$i]}
    echo -n "Checking $PKG... "
    FILE="pkgs/development/libraries/haskell/$PKG/default.nix"
    TEMPFILE=$(tempfile)
    cabal2nix cabal://$PKG >> $TEMPFILE
    if [[ -n "$(diff -q $TEMPFILE $FILE)" ]] ; then
        CHANGED+=( $PKG )
        echo "Package updated"
        cat $TEMPFILE > $FILE
    else
        echo "No changes"
    fi
    rm $TEMPFILE
done

git checkout -b $BRANCH_NAME

if [[ ${#CHANGED[@]} -ne 0 ]]; then
    git add pkgs/development/libraries/haskell
    git commit -m "Updated haskell packages ${CHANGED[@]}"
    git remote add fork $FORK
    git push fork $BRANCH_NAME
    hub pull-request
fi

cleanup
