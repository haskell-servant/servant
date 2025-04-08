#!/usr/bin/env bash

case "$(uname -s)" in
        Linux*) sudo apt update && sudo apt install postgresql liblzma-dev;;
        Darwin*) brew install postgresql openssl;;
esac
