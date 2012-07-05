#!/usr/bin/zsh

PATH=$PATH:/sbin/:/usr/sbin/:$HOME/bin/

# Configuration
xrdb -merge ~/.Xresources
setxkbmap -option caps:escape -option compose:menu -option terminate:ctrl_alt_bksp
xsetroot -cursor_name left_ptr
feh --no-fehbg --bg-scale $(find $HOME/img/misc/wallpaper -type f | shuf -n1)

# Helper daemons
keychain &
xbindkeys &
anacron -t $HOME/.anacrontab -S $HOME/.anacronspool &

# Foreground apps
iceweasel &
urxvt -e "irssi" &
urxvt -e "ncmpc" -c "--colors" &
urxvt -e "ssh-add" &
emacs &

# Windowing environment
$HOME/.dzen/dzen.sh &
exec $HOME/.cabal/bin/xmonad
