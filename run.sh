#!/bin/sh

mkdir -p src
# stack run

stack build
stack install

exit 0
