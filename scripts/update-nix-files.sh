#!/bin/bash -
#===============================================================================
#
#          FILE: update-nix-files.sh
#
#         USAGE: ./update-nix-files.sh
#
#   DESCRIPTION: Update nix files at top-level and add all packages
#
#  REQUIREMENTS: bash >= 4, cabal2nix(nix tool)
#===============================================================================

set -o nounset
set -o errexit

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
. "$DIR"/lib/common.sh
SHELLNIX="shell.nix"

write-package-shell-nix () {
    rm -rf $SHELLNIX
    echo -e "let" >> $SHELLNIX
    echo -e "  pkgs = import <nixpkgs> {};" >> $SHELLNIX
    echo -e "" >> $SHELLNIX
    echo -e "  haskellPackages = pkgs.haskellPackages.override {" >> $SHELLNIX
    echo -e "    overrides = self: super: {" >> $SHELLNIX
    for n in ${SOURCES[@]} ; do
        if [[ $1 != $n ]]; then
            echo -e "      $n = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage ../$n {}) \"--ghc-options=-Werror\");" >> $SHELLNIX
        fi
    done
    echo -e "      $1 = pkgs.haskell.lib.appendConfigureFlag (self.callPackage ./. {}) \"--ghc-options=-Werror\";" >> $SHELLNIX
    echo -e "    };" >> $SHELLNIX
    echo -e "  };" >> $SHELLNIX
    echo -e "" >> $SHELLNIX
    echo -e "in haskellPackages.$s.env" >> $SHELLNIX
}

update-nix-files () {
    for s in ${SOURCES[@]} ; do
        cd "$s" && cabal2nix . > default.nix
        write-package-shell-nix $s
    done
}

update-nix-files
