#!/usr/bin/zsh

while read LINE; do
  ACTIVE_WINDOW=$(xprop -id $(xprop -root | awk '/^_NET_ACTIVE_WINDOW/{print $5}') | awk '/^WM_NAME/{print $3}' | tr -d '"')
  INACTIVE_WINDOWS=""
  OLDIFS=$IFS  # Input Field Separator (saving state to restore after using custom one)
  IFS=$'\n'
  for WIN in $(wmctrl -l | awk "/[[:alnum:]]+ +$(wmctrl -d | awk '/*/{print $1}')/{print \$1, \$4}"); do
    IFS=" "
    if [[ ${WIN[(w)2,-1]} != ${ACTIVE_WINDOW} ]]; then
      INACTIVE_WINDOWS="$INACTIVE_WINDOWS [^ca(1,wmctrl -ia $WIN[(w)1]) ${WIN[(w)2,-1]} ^ca()] "
    fi
  done
  IFS=$OLDIFS
  print " $(echo "$LINE" $INACTIVE_WINDOWS | tr -d '\n')"
done
