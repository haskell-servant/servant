#/usr/bin/env bash

set -o errexit

cd test/ghcjs

export STACK_YAML=stack-ghc.yaml
stack setup
stack build
cp $(stack exec which testServer) .
