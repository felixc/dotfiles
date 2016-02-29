#!/bin/zsh

set -e

pwd=$(pwd)
files=(anacrontab aspell.en.pws caffrc emacs.d gitconfig gtkrc-2.0 hgrc \
       ledgerrc msmtprc muttrc notmuch-config offlineimaprc signature quiltrc \
       tmux.conf vimrc xbindkeysrc XCompose xinitrc Xresources xscreensaver \
       zshenv zshrc)

for file in $files; do;
  ln -fsT "$pwd/$file" "$HOME/.$file"
done

mkdir -p "$HOME/.config/gtk-3.0"
ln -fs "$pwd/settings.ini" "$HOME/.config/gtk-3.0/settings.ini"

mkdir -p "$HOME/.gnupg"
ln -fs "$pwd/gpg.conf" "$HOME/.gnupg/gpg.conf"

mkdir -p "$HOME/.caff/gnupghome"
ln -fs "$pwd/gpg.conf" "$HOME/.caff/gnupghome/gpg.conf"

mkdir -p "$HOME/.lbdb"
ln -fs "$pwd/lbdbrc" "$HOME/.lbdb/lbdbrc"
ln -fs "$pwd/lbdb-ldap.rc" "$HOME/.lbdb/ldap.rc"

mkdir -p "$HOME/.ipython/profile_default/"
ln -fs "$pwd/ipython_config.py" "$HOME/.ipython/profile_default/ipython_config.py"

mkdir -p "$HOME/.anacronspool"

moz_profile_dir=$(find "$HOME/.mozilla/firefox" -name "*.default" -type d || echo "")
if [ -n "$moz_profile_dir" ]; then
    ln -fs "$pwd/moz-user.js" "$moz_profile_dir/user.js"
    ln -fs "$pwd/moz-userContent.css" "$moz_profile_dir/chrome/userContent.css"
    ln -fs "$pwd/moz-userChrome.css" "$moz_profile_dir/chrome/userChrome.css"
fi

mkdir -p "$HOME/bin"
find "$pwd/bin" -type f -exec basename '{}' \; |
  while read cmd; do;
    ln -fsT "$pwd/bin/$cmd" "$HOME/bin/$cmd"
  done

ln -fsT "$HOME/.xinitrc" "$HOME/.xsession"

mkdir -p "$HOME/msg/.offlineimap"
echo '#!/bin/sh\nofflineimap -u Quiet' >! "$HOME/msg/.offlineimap/run"
chmod +x "$HOME/msg/.offlineimap/run"

chmod og-rwx "$pwd/msmtprc"

crontab "$pwd/crontab"

mkdir -p "$HOME/.xmonad"
ln -fs "$pwd/xmonad.hs" "$HOME/.xmonad/xmonad.hs"
if command -v xmonad > /dev/null; then
  xmonad --recompile && xmonad --restart
fi

# Get cert for keyserver
if [ ! -f "$HOME/.gnupg/sks-keyservers.netCA.pem" ]; then
  wget --quiet https://sks-keyservers.net/sks-keyservers.netCA.pem \
    -O "$HOME/.gnupg/sks-keyservers.netCA.pem"
fi

# Let's get some icons for use in our dzen display
if [ ! -d "$pwd/dzen/icons" ]; then;
  wget --quiet https://bitbucket.org/jerronymous/dotfiles/raw/d8b5855a5ec6f44a4e5ac9c5d6e94ea98246b7cc/.xmonad/dzen/icon-packs/xbm8x8-0.1.tar.gz
  tar -xf xbm8x8-0.1.tar.gz
  mkdir -p dzen/icons
  mv xbm8x8/* dzen/icons
  rm -rf xbm8x8*
fi
