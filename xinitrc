#!/bin/sh

PATH=$PATH:/sbin:/usr/sbin:$HOME/bin

existsp() {
  command -v $1 > /dev/null;
}

existsp x-terminal-emulator ||
  { existsp kitty && alias x-terminal-emulator=kitty; } ||
  { existsp gnome-terminal && alias x-terminal-emulator=gnome-terminal; } ||
  { alias x-terminal-emulator=xterm; }

# Configuration
setxkbmap -layout us,ru -variant ,phonetic -option grp:shifts_toggle \
    -option caps:escape -option compose:menu -option terminate:ctrl_alt_bksp &
existsp numlockx && numlockx
xsetroot -cursor_name left_ptr &
[ -f $HOME/.Xresources ] && xrdb -merge $HOME/.Xresources &
if existsp feh && [ -d $HOME/img/wallpaper ]; then
  (while true; do
     feh --no-fehbg --bg-fill $(find -L $HOME/img/wallpaper -type f | shuf -n1);
     sleep $(( 10 * 60 ));
   done) &
fi

# Helper daemons
existsp picom && picom -f -I 0.2 -O 0.2 -D 2 --inactive-dim 0.05 -b &
existsp keychain && keychain --quiet && . $HOME/.keychain/*-sh*
existsp xbindkeys && xbindkeys &
existsp xscreensaver && xscreensaver -nosplash &
existsp dunst && dunst &
existsp anacron && anacron -t $HOME/.anacrontab -S $HOME/.anacronspool &
existsp supervise && existsp offlineimap && supervise $HOME/msg/.offlineimap &
existsp emacs && emacs --daemon &

# Foreground apps
existsp ssh-add && x-terminal-emulator -e zsh -c ssh-add &
existsp mutt && x-terminal-emulator -e zsh -c mutt &
existsp steam && GDK_SCALE=2 steam &
existsp discord && discord &
existsp firefox && firefox &

# Windowing environment
[ -x $HOME/cfg/dzen/dzen.sh ] && $HOME/cfg/dzen/dzen.sh &
existsp xmonad && exec xmonad || exec x-terminal-emulator
