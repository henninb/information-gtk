#!/bin/sh

# stack run

sudo pacman --noconfirm --needed -S gobject-introspection

hlint src/Main.hs
# stack build
stack install
# stack run

exit 0
