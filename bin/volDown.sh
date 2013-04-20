#!/usr/bin/zsh

amixer -q set Master 3%-
exec updateVolumeDisplay.sh
