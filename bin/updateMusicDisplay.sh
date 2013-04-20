#!/usr/bin/zsh

source "$HOME/cfg/dzen/appearance.sh"

MUSIC_PIPE=${1:-$(ls -t /tmp/dzen-music-* | head -1)}

MUSIC_ICON="^fg($COL_HIGHLIGHT)^i($HOME/cfg/dzen/icons/note.xbm)^fg()"
PAUSE_ICON="^fg($COL_WARN)^i($HOME/cfg/dzen/icons/pause.xbm)^fg()"
PLAY_ICON="^fg($COL_EMPH)^i($HOME/cfg/dzen/icons/play.xbm)^fg()"

MUSIC_STATUS_ICON=$PAUSE_ICON
if [[ $(mpc | sed -ne "2s/\[\(\w*\)\].*/\1/p") == "playing" ]]; then
  MUSIC_STATUS_ICON=$PLAY_ICON
fi

echo "$SEPARATOR $MUSIC_ICON $MUSIC_STATUS_ICON" > $MUSIC_PIPE
