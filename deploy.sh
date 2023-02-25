#!/bin/zsh

set -e

pwd=$(pwd)
files=(aspell.en.pws caffrc emacs.d gitattributes gitconfig gitignore \
       gtkrc-2.0 ledgerrc mutt nethackrc notmuch-config offlineimaprc \
       psqlrc signature quiltrc tmux.conf vimrc xbindkeysrc XCompose \
       xinitrc Xresources xscreensaver zshenv zshrc)

for file in $files; do;
  ln -srfT "$pwd/$file" "$HOME/.$file"
done

ln -srf "$HOME/.xinitrc" "$HOME/.xsessionrc"

mkdir -p "$HOME/.config"
ln -srf "$pwd/user-dirs.dirs" "$HOME/.config/user-dirs.dirs"

mkdir -p "$HOME/.config/gtk-3.0"
ln -srf "$pwd/settings.ini" "$HOME/.config/gtk-3.0/settings.ini"
ln -srf "$pwd/gtk.css" "$HOME/.config/gtk-3.0/gtk.css"

mkdir -p "$HOME/.config/fontconfig"
ln -srf "$pwd/fonts.conf" "$HOME/.config/fontconfig/fonts.conf"

mkdir -p "$HOME/.config/dunst"
ln -srf "$pwd/dunstrc" "$HOME/.config/dunst/dunstrc"

mkdir -p "$HOME/.config/kitty"
ln -srf "$pwd/kitty.conf" "$HOME/.config/kitty/kitty.conf"

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

mkdir -p "$HOME/.local/state/anacronspool"

moz_profile_dir=$(find "$HOME/.mozilla/firefox" -name "*.default-beta" -type d 2> /dev/null || echo "")
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

crontab "$pwd/crontab"

mkdir -p "$HOME/.xmonad"
ln -srf "$pwd/xmonad.hs" "$HOME/.xmonad/xmonad.hs"
if command -v xmonad > /dev/null; then
  xmonad --recompile
fi
