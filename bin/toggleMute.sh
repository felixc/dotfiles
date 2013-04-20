#!/usr/bin/zsh

IS_MUTED=$(amixer get Master | sed -ne "/Playback/ s/.* \[\(.*\)\]/\1/p" | head -n 1)

if [[ $IS_MUTED == "on" ]]; then
  amixer -q set Master mute
else
  amixer -q set Master unmute
fi

exec updateVolumeDisplay.sh
