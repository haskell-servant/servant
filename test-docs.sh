#!/bin/sh

doctest -isrc -optP-include -optPdist/build/autogen/cabal_macros.h $(find src/ -name '*.hs')
