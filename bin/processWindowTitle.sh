#!/bin/sh

command -v xprop wmctrl > /dev/null 2>&1 || {
  while read -r LINE; do echo "$LINE"; done
}

while read -r LINE; do
  ACTIVE_WINDOW=$(printf "%#010x\n" "$(xprop -root | sed -n 's/^_NET_ACTIVE_WINDOW.* \(.*\)/\1/p')")
  ACTIVE_WORKSPACE=$(wmctrl -d | awk '/*/{print $1}')
  INACTIVE_WINDOWS=$(wmctrl -l | sed -n -e "/^$ACTIVE_WINDOW/d" -e "s/^0x.* $ACTIVE_WORKSPACE \w* \(.*\)$/[ \1 ]/p" | tr '\n' ' ')
  echo " $LINE $INACTIVE_WINDOWS"
done
