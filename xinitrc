#!/bin/sh

. /home/felix/.zshenv

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

# Desktop environment
existsp picom && picom \
  --experimental-backends --backend glx \
  --blur-method dual_kawase --blur-strength 3 --blur-background-exclude '!class_g = "kitty"' \
  --shadow --shadow-exclude 'window_type *= "menu" || window_type = "dock" || window_type = "tooltip" || window_type = "utility"' \
  --corner-radius 8 --rounded-corners-exclude 'window_type *= "dock"' \
  --fading --fade-in-step 0.10 --fade-out-step 0.10 \
  --inactive-dim 0.05 \
  --daemon &
$HOME/bin/dock-helpers/startup.sh &
$HOME/wrk/eww/target/release/eww open main-monitor-top-bar &
existsp xscreensaver && xscreensaver -nosplash &
existsp dunst && dunst &

# Auto-start tools and utilities
existsp keychain && keychain --quiet && . $HOME/.keychain/*-sh*
existsp xbindkeys && xbindkeys &
existsp supervise && existsp offlineimap && supervise $HOME/msg/.offlineimap &
existsp emacs && emacs --daemon &

# Foreground apps
existsp ssh-add && x-terminal-emulator -e zsh -c ssh-add &
existsp mutt && x-terminal-emulator -e zsh -c mutt &
existsp steam && GDK_SCALE=2 steam &
existsp firefox && firefox &

# Window manager
existsp xmonad && exec xmonad || exec x-terminal-emulator
