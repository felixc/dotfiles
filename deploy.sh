#!/bin/zsh

set -e

pwd=$(pwd)
files=(anacrontab aspell.en.pws caffrc emacs.d fonts.conf gitconfig gtkrc-2.0 \
       hgrc ledgerrc msmtprc mutt notmuch-config offlineimaprc signature \
       quiltrc tmux.conf vimrc xbindkeysrc XCompose xinitrc Xresources \
       xscreensaver zshenv zshrc)

for file in $files; do;
  ln -srfT "$pwd/$file" "$HOME/.$file"
done

ln -srfT "$HOME/.xinitrc" "$HOME/.xsessionrc"

mkdir -p "$HOME/.config/gtk-3.0"
ln -srf "$pwd/settings.ini" "$HOME/.config/gtk-3.0/settings.ini"
ln -srf "$pwd/gtk.css" "$HOME/.config/gtk-3.0/gtk.css"

mkdir -p "$HOME/.gnupg"
ln -srf "$pwd/gpg.conf" "$HOME/.gnupg/gpg.conf"
ln -srf "$pwd/scdaemon.conf" "$HOME/.gnupg/scdaemon.conf"
ln -srf "$pwd/gpg-agent.conf" "$HOME/.gnupg/gpg-agent.conf"
find "$HOME/.gnupg/" -type f -exec chmod 600 '{}' \;
find "$HOME/.gnupg/" -type d -exec chmod 700 '{}' \;

mkdir -p "$HOME/.caff/gnupghome"
ln -srf "$pwd/gpg.conf" "$HOME/.caff/gnupghome/gpg.conf"

mkdir -p "$HOME/.lbdb"
ln -srf "$pwd/lbdbrc" "$HOME/.lbdb/lbdbrc"
ln -srf "$pwd/lbdb-ldap.rc" "$HOME/.lbdb/ldap.rc"

mkdir -p "$HOME/.ipython/profile_default/"
ln -srf "$pwd/ipython_config.py" "$HOME/.ipython/profile_default/ipython_config.py"

mkdir -p "$HOME/.ssh"
ln -srf "$pwd/ssh-config" "$HOME/.ssh/config"

mkdir -p "$HOME/.anacronspool"

moz_profile_dir=$(find "$HOME/.mozilla/firefox" -name "*.default" -type d 2> /dev/null)
if [ -n "$moz_profile_dir" ]; then
    mkdir -p "$moz_profile_dir/chrome"
    ln -srf "$pwd/moz-user.js" "$moz_profile_dir/user.js"
    ln -srf "$pwd/moz-userContent.css" "$moz_profile_dir/chrome/userContent.css"
    ln -srf "$pwd/moz-userChrome.css" "$moz_profile_dir/chrome/userChrome.css"
fi

ln -srfT "$pwd/bin" "$HOME/bin"

mkdir -p "$HOME/msg/.offlineimap"
echo '#!/bin/sh\nofflineimap -u Quiet' >! "$HOME/msg/.offlineimap/run"
chmod +x "$HOME/msg/.offlineimap/run"

chmod og-rwx "$pwd/msmtprc"

crontab "$pwd/crontab"

mkdir -p "$HOME/.xmonad"
ln -srf "$pwd/xmonad.hs" "$HOME/.xmonad/xmonad.hs"
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
