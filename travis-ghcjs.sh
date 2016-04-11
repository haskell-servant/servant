#/usr/bin/env bash
set -ev

# tinc
#cabal exec which hspec-discover
barf


cabal install hspec-discover --prefix $HOME/.local
# export PATH=$HOME/huhu/bin:$PATH
which hspec-discover

cd servant-client
./test/ghcjs/run-tests.sh
