#!/bin/sh

set -eu

WORKSPACES_AND_WINDOWS_PIPE="/tmp/dock-helper-pipes/workspaces-and-windows"

# Fallback in case wmctrl is not installed
command -v xprop wmctrl > /dev/null 2>&1 || {
  while read -r WORKSPACES; do echo "$WORKSPACES" > "$WORKSPACES_AND_WINDOWS_PIPE"; done
}

while read -r WORKSPACES; do
  ACTIVE_WINDOW_ID=$(printf "%#010x\n" "$(xprop -root | sed -n 's/^_NET_ACTIVE_WINDOW.* \(.*\)/\1/p')")
  ACTIVE_WINDOW_RAW=$(wmctrl -l | sed -n -e "s/^$ACTIVE_WINDOW_ID  \w\+ \w\+ \(.\+\)$/\1/p")
  if [ -z "$ACTIVE_WINDOW_RAW" ]; then
    ACTIVE_WINDOW_FORMATTED=""
  else
    ACTIVE_WINDOW_TRIMMED=$(echo "$ACTIVE_WINDOW_RAW" | sed -e "s/\(.\{100\}\)....\+/\1…/" )
    ACTIVE_WINDOW_FORMATTED=$(echo "$ACTIVE_WINDOW_TRIMMED" | sed -e "s/\(.*\)/[ \1 ]/")
  fi

  ACTIVE_WORKSPACE=$(wmctrl -d | awk '/*/{print $1}')
  INACTIVE_WINDOWS_RAW=$(wmctrl -l | sed -n -e "/^$ACTIVE_WINDOW_ID/d" -e "s/^0x.* $ACTIVE_WORKSPACE \w* \(.*\)$/\1/p")
  if [ -z "$INACTIVE_WINDOWS_RAW" ]; then
    INACTIVE_WINDOWS_FORMATTED=""
  else
    INACTIVE_WINDOWS_TRIMMED=$(echo "$INACTIVE_WINDOWS_RAW" | sed -e "s/\(.\{60\}\)....\+/\1…/")
    INACTIVE_WINDOWS_FORMATTED=$(echo "$INACTIVE_WINDOWS_TRIMMED" | sed -e "s/\(.*\)/[ \1 ]/" | tr '\n' ' ')
  fi

  # TODO: Limit max number of windows shown.

  echo " $WORKSPACES ▪ $ACTIVE_WINDOW_FORMATTED $INACTIVE_WINDOWS_FORMATTED" > "$WORKSPACES_AND_WINDOWS_PIPE"
done
