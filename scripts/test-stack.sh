#!/usr/bin/env bash

set -o nounset
set -o errexit

for stack_file in stack*.yaml ; do
  echo testing $stack_file...
  export STACK_YAML=$stack_file
  stack setup
  stack test --fast --ghc-options="-Werror"
done
