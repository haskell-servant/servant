#!/usr/bin/env bash

set -o errexit

loc=$(dirname $0)
cd $loc

cd ../../

npm install xhr2

export STACK_YAML=test/ghcjs/stack-ghcjs.yaml
stack setup
# stack build
# stack test --fast
