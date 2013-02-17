#!/bin/sh

existsp() {
  command -v $1 > /dev/null;
}

existsp x-terminal-emulator ||
  { existsp urxvt && alias x-terminal-emulator=urxvt; } ||
  { existsp gnome-terminal && alias x-terminal-emulator=gnome-terminal; } ||
  { alias x-terminal-emulator=xterm; }

PATH=$PATH:/sbin/:/usr/sbin/:$HOME/bin/

# Configuration
setxkbmap -option caps:escape -option compose:menu -option terminate:ctrl_alt_bksp &
xsetroot -cursor_name left_ptr &
[ -f $HOME/.Xresources ] && xrdb -merge $HOME/.Xresources &
if existsp feh && [ -d $HOME/img/wallpaper ]; then
  (while true; do
     feh --no-fehbg --bg-scale $(find $HOME/img/wallpaper -type f | shuf -n1);
     sleep $(( 10 * 60 ));
   done) &
fi

# Helper daemons
existsp keychain && keychain &
existsp xbindkeys && xbindkeys &
existsp xscreensaver && xscreensaver -nosplash &
existsp anacron && anacron -t $HOME/.anacrontab -S $HOME/.anacronspool &
existsp unclutter && unclutter -idle 2 -jitter 2 &

# Foreground apps
existsp firefox && firefox &
existsp irssi && x-terminal-emulator -e irssi &
existsp ssh-add && x-terminal-emulator -e ssh-add &

# Windowing environment
[ -x $HOME/cfg/dzen/dzen.sh ] && $HOME/cfg/dzen/dzen.sh &
[ -x $HOME/.cabal/bin/xmonad ] && exec $HOME/.cabal/bin/xmonad ||
  { existsp xmonad && exec xmonad; } ||
  { exec x-terminal-emulator; }
