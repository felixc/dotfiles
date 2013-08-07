#!/bin/zsh

set -e

pwd=$(pwd)
files=(caffrc emacs.d gitconfig gtkrc-2.0 msmtprc muttrc offlineimaprc \
       quiltrc tmux.conf vimrc xbindkeysrc xinitrc xmonad Xresources zshrc)

for file in $files; do;
  ln -fsT "$pwd/$file" "$HOME/.$file"
done

mkdir -p "$HOME/.config/gtk-3.0"
ln -fs "$pwd/settings.ini" "$HOME/.config/gtk-3.0/settings.ini"

mkdir -p "$HOME/.gnupg"
ln -fs "$pwd/gpg.conf" "$HOME/.gnupg/gpg.conf"

ln -fsT "$HOME/.xinitrc" "$HOME/.xsession"

mkdir -p "$HOME/msg/.offlineimap"
echo '#!/bin/sh\nofflineimap' >! "$HOME/msg/.offlineimap/run"
chmod +x "$HOME/msg/.offlineimap/run"

# Set up scripts and tools
mkdir -p "$HOME/bin"
find "$pwd/bin" -type f -exec basename '{}' \; |
  while read cmd; do;
    ln -fsT "$pwd/bin/$cmd" "$HOME/bin/$cmd"
  done

# For now, we need to build dzen from source, since the version in the
# Debian repositories is rather outdated.
if ( ! command -v dzen2 > /dev/null ); then
  svn checkout http://dzen.googlecode.com/svn/trunk/ dzen-bin
  cd dzen-bin
  sudo make clean install
  cd gadgets
  sudo make clean install
  cd $pwd
  rm -rf dzen-bin
fi

# Let's get some icons for use in our dzen display
if [ ! -d "$pwd/dzen/icons" ]; then;
  wget --quiet http://bitbucket.org/jerronymous/dotfiles/src/d8b5855a5ec6/.xmonad/dzen/icon-packs/xbm8x8-0.1.tar.gz
  tar -xf xbm8x8-0.1.tar.gz
  mkdir -p dzen/icons
  mv xbm8x8/* dzen/icons
  rm -rf xbm8x8*
fi

# Before we can install packages for Emacs23, we need a package manager
if [ ! -f "$pwd/emacs.d/package.el" ]; then
  wget --quiet -O "$pwd/emacs.d/package.el" \
    $(wget --quiet -O - http://marmalade-repo.org/ | \
        sed -n 's|.*\(http://.*package.el\)">|\1|p')
fi
