#!/bin/sh

# stack run

hlint src/Main.hs
# stack build
stack install
# stack run

exit 0
