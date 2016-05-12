#/usr/bin/env bash

set -o errexit

cd test/ghcjs/testServer

unset STACK_YAML
stack setup
stack build
cp $(stack exec which testServer) .
