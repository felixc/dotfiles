#!/bin/sh

set -eu

case $1 in
    up)
        pamixer --unmute --increase 3
    ;;
    down)
        pamixer --decrease 3
    ;;
    set)
        if [ "$(pamixer --get-volume)" -lt "$2" ]; then
          pamixer --unmute
        fi
        pamixer --set-volume "$2"
    ;;
    mute)
        pamixer --toggle-mute
    ;;
esac

exec ~/bin/dock-helpers/volume-display.sh
