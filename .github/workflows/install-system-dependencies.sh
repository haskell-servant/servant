#!/usr/bin/env bash

case "$(uname -s)" in
        Linux*) sudo apt install postgresql;;
        Darwin*) brew install postgresql openssl;;
esac
