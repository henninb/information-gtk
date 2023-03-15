#!/bin/sh

if [ "$OS" = "Arch Linux" ] || [ "$OS" = "Manjaro Linux" ] || [ "$OS" = "ArcoLinux" ]; then
  sudo pacman --noconfirm --needed -S gobject-introspection
  sudo pacman --noconfirm --needed -S gobject-introspection-runtime
  sudo pacman --noconfirm --needed -S gdk-pixbuf2
  sudo pacman --noconfirm --needed -S harfbuzz
  sudo pacman --noconfirm --needed -S harfbuzz-cairo
elif [ "$OS" = "Gentoo" ]; then
  sudo emerge --update --newuse harfbuzz
elif [ "$OS" = "Linux Mint" ] || [ "$OS" = "Ubuntu" ] || [ "$OS" = "Raspbian GNU/Linux" ]; then
  sudo apt install -y gobject-introspection
  sudo apt install -y libatk1.0-dev
  sudo apt install -y libpango1.0-dev
  sudo apt install -y libgdk-pixbuf-2.0-dev
  sudo apt install -y libgirepository1.0-dev
  sudo apt install -y libgtk-3-dev
elif [ "$OS" = "Void" ]; then
  sudo xbps-install -y gobject-introspection
elif [ "$OS" = "FreeBSD" ]; then
  echo "sudo pkg install -y"
elif [ "$OS" = "Solus" ]; then
  "sudo eopkg install -y"
elif [ "$OS" = "openSUSE Tumbleweed" ]; then
  sudo zypper install -y gobject-introspection-devel
  sudo zypper install -y pango-devel
elif [ "$OS" = "Fedora Linux" ]; then
  sudo dnf install -y gobject-introspection-devel
  sudo dnf install -y cairo-gobject-devel
  sudo dnf install -y atk-devel
  sudo dnf install -y gdk-pixbuf2-devel
  sudo dnf install -y pango-devel
  sudo dnf install -y gtk3-devel
elif [ "$OS" = "Clear Linux OS" ]; then
  "sudo swupd bundle-add"
elif [ "$OS" = "Darwin" ]; then
  echo "brew install"
else
  echo "$OS is not yet implemented."
  exit 1
fi

hlint src/Main.hs
# stack build
stack install
# stack run

exit 0
