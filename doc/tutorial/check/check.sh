#!/usr/bin/env bash

set -o errexit

# tinc

cabal exec -- ghc -Wall -Werror -outputdir build-output ../api-type.lhs -O0 -c -pgmL markdown-unlit
#cabal exec -- ghc -Wall -Werror -outputdir build-output ../server.lhs -O0 -c -fno-warn-missing-methods -fno-warn-name-shadowing
#cabal exec -- ghc -Wall -Werror -outputdir build-output ../client.lhs -O0 -c -fno-warn-missing-methods -fno-warn-name-shadowing
#cabal exec -- ghc -Wall -Werror -outputdir build-output ../javascript.lhs -O0 -c -fno-warn-missing-methods
#cabal exec -- ghc -Wall -Werror -ibuild-output -outputdir build-output ../docs.lhs -O0 -c -fno-warn-missing-methods
