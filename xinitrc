#!/bin/sh

PATH=$PATH:/sbin/:/usr/sbin/:$HOME/bin/

existsp() {
  command -v $1 > /dev/null;
}

existsp x-terminal-emulator ||
  { existsp urxvt && alias x-terminal-emulator=urxvt; } ||
  { existsp gnome-terminal && alias x-terminal-emulator=gnome-terminal; } ||
  { alias x-terminal-emulator=xterm; }

# Configuration
setxkbmap -layout us,ru -variant ,phonetic -option grp:shifts_toggle \
    -option caps:escape -option compose:menu -option terminate:ctrl_alt_bksp &
xsetroot -cursor_name left_ptr &
[ -f $HOME/.Xresources ] && xrdb -merge $HOME/.Xresources &
if existsp feh && [ -d $HOME/img/wallpaper ]; then
  (while true; do
     feh --no-fehbg --bg-scale $(find -L $HOME/img/wallpaper -type f | shuf -n1);
     sleep $(( 10 * 60 ));
   done) &
fi

# Helper daemons
existsp keychain && keychain -q &
existsp xbindkeys && xbindkeys &
existsp xscreensaver && xscreensaver -nosplash &
existsp compton && compton -c -C -f -D 1 -r 10 --inactive-dim 0.05 \
    -z -G --shadow-exclude 'class_g = "dzen"' -b &
existsp anacron && anacron -t $HOME/.anacrontab -S $HOME/.anacronspool &
existsp supervise && existsp offlineimap && supervise $HOME/msg/.offlineimap &
existsp git-annex && git-annex assistant --autostart &

# Foreground apps
existsp firefox && firefox &
existsp ssh-add && x-terminal-emulator -e zsh -c ssh-add &
existsp mutt && x-terminal-emulator -e zsh -c mutt &

# Windowing environment
[ -x $HOME/cfg/dzen/dzen.sh ] && $HOME/cfg/dzen/dzen.sh &
[ -x $HOME/.cabal/bin/xmonad ] && exec $HOME/.cabal/bin/xmonad ||
  { existsp xmonad && exec xmonad; } ||
  { exec x-terminal-emulator; }
