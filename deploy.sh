#!/bin/zsh

set -e

pwd=$(pwd)
files=(emacs.d gitconfig gtkrc-2.0 msmtprc muttrc offlineimaprc \
       quiltrc tmux.conf vimrc xbindkeysrc xinitrc xmonad Xresources zshrc)

for file in $files; do;
  ln -isT "$pwd/$file" "$HOME/.$file"
done

ln -isT "$HOME/.xinitrc" "$HOME/.xsession"

# Set up scripts and tools
mkdir -p "$HOME/bin"
find "$pwd/bin" -type f -exec basename '{}' \; |
  while read cmd; do;
    ln -isT "$pwd/bin/$cmd" "$HOME/bin/$cmd"
  done

# For now, we need to build dzen from source, since the version in the
# Debian repositories is rather outdated.
svn checkout http://dzen.googlecode.com/svn/trunk/ dzen-bin
cd dzen-bin
sudo make clean install
cd gadgets
sudo make clean install
cd $pwd
rm -rf dzen-bin

# Let's get some icons for use in our dzen display
wget --quiet http://bitbucket.org/jerronymous/dotfiles/src/d8b5855a5ec6/.xmonad/dzen/icon-packs/xbm8x8-0.1.tar.gz
tar -xf xbm8x8-0.1.tar.gz
mkdir -p dzen/icons
mv xbm8x8/* dzen/icons
rm -rf xbm8x8*

# Before we can install packages for Emacs23, we need a package manager
wget --quiet -O "$pwd/emacs.d/package.el" \
  $(wget --quiet -O - http://marmalade-repo.org/ | \
      sed -n 's|.*\(http://.*package.el\)">|\1|p')
