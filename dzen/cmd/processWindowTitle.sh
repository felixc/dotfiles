#!/usr/bin/zsh

command -v xprop wmctrl > /dev/null 2>&1 || {
  while read -e; do; done
}

while read LINE; do
  ACTIVE_WINDOW=$(printf "%#010x\n" $(xprop -root | sed -n 's/^_NET_ACTIVE_WINDOW.* \(.*\)/\1/p'))
  INACTIVE_WINDOWS=$(wmctrl -l | sed -n -e "/^$ACTIVE_WINDOW/d" -e "s/^0x.*  $(wmctrl -d | awk '/*/{print $1}') \w* \(.*\)$/[ \1 ]/p" | tr '\n' ' ')
  echo "$LINE $INACTIVE_WINDOWS"
done
