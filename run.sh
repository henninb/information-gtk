#!/bin/sh

# stack run

sudo pacman --noconfirm --needed -S gobject-introspection
sudo dnf install -y gobject-introspection-devel
sudo dnf install -y cairo-gobject-devel

hlint src/Main.hs
# stack build
stack install
# stack run

exit 0
