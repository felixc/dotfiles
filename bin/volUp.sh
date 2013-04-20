#!/usr/bin/zsh

amixer -q set Master 3%+ unmute
exec updateVolumeDisplay.sh
