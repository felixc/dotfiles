#!/usr/bin/zsh

source "$HOME/cfg/dzen/appearance.sh"

# Necessary to permit invocation outside of the timed update process.
VOL_PIPE=${1:-$(ls -t /tmp/dzen-volume-* | head -1)}

VOL_ICON_SPEAKER="^fg($COL_HIGHLIGHT)^i($HOME/cfg/dzen/icons/spkr_01.xbm)^fg()"
VOL_ICON_MUTED="^fg($COL_WARN)^i($HOME/cfg/dzen/icons/spkr_02.xbm)^fg()"

VOL_CURRENT=$(amixer get Master | sed -ne "/Playback/ s/.* \[\(.*\)%\].*/\1/p" | head -n 1)
VOL_ISMUTED=$(amixer get Master | sed -ne "/Playback/ s/.* \[\(.*\)\]/\1/p" | head -n 1)

VOL_GRAPH_COL=$COL_HIGHLIGHT
VOL_ICON=$VOL_ICON_SPEAKER
if [[ $VOL_ISMUTED == "off" ]]; then
  VOL_GRAPH_COL=$COL_WARN
  VOL_ICON=$VOL_ICON_MUTED
fi;

VOL_GRAPH=$(echo $VOL_CURRENT | dzen2-gdbar -h 10 -ss 1 -w 48 -sw 4 -s o -nonl -bg $COL_GRAPH_BORDER -fg $VOL_GRAPH_COL)

echo "$SEPARATOR $VOL_ICON $VOL_CURRENT% $VOL_GRAPH" > $VOL_PIPE
