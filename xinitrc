#!/bin/sh

. $HOME/.zshenv

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
  --backend glx \
  --blur-method dual_kawase --blur-strength 3 --blur-background-exclude '!class_g = "kitty" && !name *= "main-monitor-top-bar"' \
  --shadow --shadow-exclude 'window_type *= "menu" || window_type = "dock" || window_type = "tooltip" || window_type = "utility" || fullscreen' \
  --fading --fade-in-step 0.10 --fade-out-step 0.10 \
  --inactive-dim 0.05 \
  --corner-radius 8 --rounded-corners-exclude 'window_type *= "dock"' \
  --daemon &
existsp xscreensaver && xscreensaver -nosplash &

# Auto-start tools and utilities
existsp xbindkeys && xbindkeys &
existsp supervise && existsp offlineimap && supervise $HOME/msg/.offlineimap &
existsp emacs && emacs --daemon &

# Foreground apps
existsp mutt && x-terminal-emulator -e zsh -c mutt &
existsp firefox && firefox &

# Window manager
existsp xmonad && exec xmonad || exec x-terminal-emulator
