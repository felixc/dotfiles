#!/usr/bin/zsh

set -eu


DZEN_DIR="$HOME/cfg/dzen"

source "$DZEN_DIR/appearance.sh"

DZEN=(dzen2 -h $DOCK_HEIGHT -p)


#
# Temporary pipes
#
BAT_PIPE="/tmp/dzen-battery-$RANDOM"
NET_PIPE="/tmp/dzen-net-$RANDOM"
TIME_PIPE="/tmp/dzen-time-$RANDOM"
VOL_PIPE="/tmp/dzen-volume-$RANDOM"
TMP_FILES=($BAT_PIPE $NET_PIPE $TIME_PIPE $VOL_PIPE)


#
# Cleanup on exit
#
TRAPINT() {
  rm -f $TMP_FILES
  for CHILD in $(ps -o pid --no-heading -s $(ps -o sess --no-heading --pid $$)); do
    if (( CHILD != $$ )); then
      kill $CHILD 2> /dev/null
    fi
  done
  return $(( 128 + $1 ))
}


#
# Battery Configuration
#
BAT_ICON_LOW="^fg($COL_WARN)^i($DZEN_DIR/icons/bat_empty_02.xbm)^fg()"
BAT_ICON_MED="^fg($COL_HIGHLIGHT)^i($DZEN_DIR/icons/bat_low_02.xbm)^fg()"
BAT_ICON_HIGH="^fg($COL_EMPH)^i($DZEN_DIR/icons/bat_full_02.xbm)^fg()"
BAT_ICON_CHARGING="^fg($COL_HIGHLIGHT)^i($DZEN_DIR/icons/ac.xbm)^fg()"

BAT_LOW_VAL=15
BAT_FULL_VAL=90

if [[ -x /usr/bin/acpi ]]; then
  BAT_EXISTS=$(acpi 2> /dev/null | grep "Battery" > /dev/null && echo 1 || echo 0)
else
  BAT_EXISTS=0
fi

#
#  Network Monitor Configuration
#
NET_ICON="^fg($COL_HIGHLIGHT)^i($DZEN_DIR/icons/wifi_01.xbm)^fg()"
NET_TX_ICON="^fg(#dfae96)^i($DZEN_DIR/icons/net_up_03.xbm)^fg()"
NET_RX_ICON="^fg($COL_HIGHLIGHT)^i($DZEN_DIR/icons/net_down_03.xbm)^fg()"

NET_RX_BYTES_OLD=0
NET_TX_BYTES_OLD=0

#
# Clock Configuration
#
TIME_ICON="^fg($COL_HIGHLIGHT)^i($DZEN_DIR/icons/clock.xbm)^fg()"
TIME_SLAVE_LINES=12
TIME_SLAVE_WIDTH=940
TIME_FORMAT='%a %d %b %Y %H:%M'

TIME_ZONES=(America/Toronto America/Chicago America/Los_Angeles)
TIME_ZONE_NAMES=(Ontario Texas California)


#
# Layout
#
SCREEN_WIDTH=$(xrandr --current 2> /dev/null | sed -ne "s/.* current \([0-9]\{4\}\).*/\1/p")
MARGIN_LEFT=$SCREEN_WIDTH

SESSION_WIDTH=44
SESSION_XPOS=$(( MARGIN_LEFT - SESSION_WIDTH ))
MARGIN_LEFT=$(( MARGIN_LEFT - SESSION_WIDTH ))

TIME_WIDTH=330
TIME_XPOS=$(( MARGIN_LEFT - TIME_WIDTH ))
MARGIN_LEFT=$(( MARGIN_LEFT - TIME_WIDTH ))

VOL_WIDTH=200
VOL_XPOS=$(( MARGIN_LEFT - VOL_WIDTH ))
MARGIN_LEFT=$(( MARGIN_LEFT - VOL_WIDTH ))

if (( $BAT_EXISTS )); then
  BAT_WIDTH=170
else
  BAT_WIDTH=0
fi
BAT_XPOS=$(( MARGIN_LEFT - BAT_WIDTH ))
MARGIN_LEFT=$(( MARGIN_LEFT - BAT_WIDTH ))

NET_WIDTH=500
NET_XPOS=$(( MARGIN_LEFT - NET_WIDTH ))
MARGIN_LEFT=$(( MARGIN_LEFT - NET_WIDTH ))


#
# Timing Intervals and Counters
#
BAT_INTERVAL=150
BAT_COUNTER=$BAT_INTERVAL

NET_INTERVAL=15
NET_COUNTER=$NET_INTERVAL

TIME_INTERVAL=30
TIME_COUNTER=$TIME_INTERVAL

TIME_CAL_INTERVAL=300
TIME_CAL_COUNTER=$TIME_CAL_INTERVAL


#
# Session Management Configuration
#
SESSION_MENU="^fg($COL_WARN)^r(13x13)^fg()\npoweroff \nreboot \nsystemctl suspend \nsystemctl hibernate "
(echo $SEPARATOR $SESSION_MENU | ${DZEN[@]} -l 4 -ta l -sa r -m -tw $SESSION_WIDTH -w 280 -x $SESSION_XPOS) &


#
# Volume Controls
#
mkfifo $VOL_PIPE
(tail -f $VOL_PIPE | ${DZEN[@]} -ta l -tw $VOL_WIDTH -x $VOL_XPOS -e "button4=exec:volUp.sh;button5=exec:volDown.sh;button2=exec:toggleMute.sh") &
updateVolumeDisplay.sh $VOL_PIPE


#
# Main loop
#
sleep 1
while true; do

  #
  # Time
  #
  if (( $TIME_COUNTER >= $TIME_INTERVAL )); then
    if [[ ! -p $TIME_PIPE ]]; then
      mkfifo $TIME_PIPE
      (tail -n 13 -f $TIME_PIPE | ${DZEN[@]} -ta l -sa c -tw $TIME_WIDTH -w $TIME_SLAVE_WIDTH -l $TIME_SLAVE_LINES -x $TIME_XPOS -e "button1=togglecollapse") &
    fi

    echo "^tw()$SEPARATOR $TIME_ICON "$(date +$TIME_FORMAT) > $TIME_PIPE

    TIME_COUNTER=0
  else
    TIME_COUNTER=$(( $TIME_COUNTER + 1 ))
  fi

  #
  # Date
  #
  if (( $TIME_CAL_COUNTER >= $TIME_CAL_INTERVAL )); then
    TIME_CAL_OUTPUT=$(ncal -b -h -A 2 | tail -n 8 | sed -r -e "1,2 s/.*/^fg(white)&^fg()/")

    TIME_TZS_OUTPUT=""
    for ((i=1; i <= ${#TIME_ZONES}; i++)) {
      TIME_NEW_TZ=$(TZ=${TIME_ZONES[i]} date +'%H:%M')
      TIME_TZS_OUTPUT="$TIME_TZS_OUTPUT^fg(white)${TIME_ZONE_NAMES[i]}^fg(): $TIME_NEW_TZ  "
    }

    echo "\n$TIME_CAL_OUTPUT\n\n $TIME_TZS_OUTPUT\n" > $TIME_PIPE

    TIME_CAL_COUNTER=0
  else
    TIME_CAL_COUNTER=$(( $TIME_CAL_COUNTER + 1 ))
  fi

  #
  # Network
  #
  if (( $NET_COUNTER >= $NET_INTERVAL )); then
    NET_COUNTER=0
    NET_INTERFACE=$(route -n | grep -E " UGH? " | awk 'END {print $8}')
    if [[ $NET_INTERFACE != "" ]]; then
      NET_IS_WIFI=0
      if [[ -x /sbin/iw && $NET_INTERFACE == $(/sbin/iw dev | sed -n 's/.*Interface \(.*\)/\1/p') ]]; then
        NET_IS_WIFI=1
      fi

      if [[ ! -p $NET_PIPE ]]; then
        mkfifo $NET_PIPE
        NET_SUB_WINDOW_LINES=$((( $NET_IS_WIFI )) && echo 3 || echo 1)
        (tail -f $NET_PIPE | ${DZEN[@]} -ta r -sa l -tw $NET_WIDTH -x $NET_XPOS -l $NET_SUB_WINDOW_LINES -w 260 -e "button1=togglecollapse") &
      fi

      NET_TX_BYTES=$(cat /sys/class/net/$NET_INTERFACE/statistics/tx_bytes)
      NET_RX_BYTES=$(cat /sys/class/net/$NET_INTERFACE/statistics/rx_bytes)

      NET_RX_RATE=$(( ($NET_RX_BYTES - $NET_RX_BYTES_OLD) / 1024 / $NET_INTERVAL ))
      NET_TX_RATE=$(( ($NET_TX_BYTES - $NET_TX_BYTES_OLD) / 1024 / $NET_INTERVAL ))
      if (( $NET_RX_RATE > 999 )); then
        NET_RX_RATE=$(echo "scale=1; $NET_RX_RATE / 1024" | bc)"MB/s"
      else
        NET_RX_RATE="${NET_RX_RATE}kB/s"
      fi
      if (( $NET_TX_RATE > 999 )); then
        NET_TX_RATE=$(echo "scale=1; $NET_TX_RATE / 1024" | bc)"MB/s"
      else
        NET_TX_RATE="${NET_TX_RATE}kB/s"
      fi

      NET_IP=$(/bin/ip addr show $NET_INTERFACE scope global | sed -ne 's/.*inet \([^ ]*\)\/.*/\1/p')

      if (( $NET_IS_WIFI )); then
        NET_ESSID=$(/sbin/iw dev $NET_INTERFACE link | sed -ne "s/.*SSID: \(.*\)/\1/p")
        NET_LINK_QUALITY=$(/sbin/iw dev $NET_INTERFACE link | sed -n 's/.*signal: -\(.*\) .*/\1/p')
        NET_BIT_RATE=$(/sbin/iw dev $NET_INTERFACE link | sed -ne "s/.*bitrate: \([0-9.]*\) MBit.*/\1/p")" Mb/s"

        # Converting dB log scale from 84 to 63 (minus sign dropped) into a linear 0–5 one:
        # 5 * (1 - (ln(x) - ln(63))/(ln(84) - ln(63))) = 77.0089 - ln(x)/0.0575
        NET_GRAPH=$(echo "77.0089 - l($NET_LINK_QUALITY)/0.0575" | bc -l | dzen2-gdbar -max 5 -min 0 -h 20 -ss 1 -w 33 -sw 5 -s o -nonl -bg $COL_GRAPH_BORDER -fg $COL_HIGHLIGHT)

        echo "^tw()$SEPARATOR $NET_ICON $NET_GRAPH $NET_RX_RATE $NET_RX_ICON  $NET_TX_RATE $NET_TX_ICON " > $NET_PIPE
        echo "  ESSID: $NET_ESSID\n  IP: $NET_IP\n  Bit Rate: $NET_BIT_RATE" > $NET_PIPE
      else
        echo "^tw()$SEPARATOR $NET_INTERFACE: $NET_RX_RATE $NET_RX_ICON  $NET_TX_RATE $NET_TX_ICON " > $NET_PIPE
        echo "  IP: $NET_IP" > $NET_PIPE
      fi

      NET_RX_BYTES_OLD=$NET_RX_BYTES
      NET_TX_BYTES_OLD=$NET_TX_BYTES
    fi
  else
    NET_COUNTER=$(( $NET_COUNTER + 1 ))
  fi

  #
  # Battery
  #
  if (( $BAT_EXISTS )) && (( $BAT_COUNTER >= $BAT_INTERVAL )); then
    if [[ ! -p $BAT_PIPE ]]; then
      mkfifo $BAT_PIPE
      (tail -n 13 -f $BAT_PIPE | ${DZEN[@]} -ta r -tw $BAT_WIDTH -x $BAT_XPOS) &
    fi

    BAT_STATE=$(acpi | awk '{print $3}')
    BAT_PCT_REMAINING=$(acpi | sed -ne "s/.* \([0-9]*\)%.*/\1/p")
    BAT_TIME_REMAINING=$(acpi | sed -ne "s/.*\(..\):\(..\):.*/\1:\2/p")

    BAT_GRAPH_COL=$COL_EMPH
    BAT_ICON=$BAT_ICON_HIGH
    if (( $BAT_PCT_REMAINING < $BAT_FULL_VAL )); then
      BAT_GRAPH_COL=$COL_HIGHLIGHT
      BAT_ICON=$BAT_ICON_MED
    fi
    if (( $BAT_PCT_REMAINING <= $BAT_LOW_VAL )); then
      BAT_GRAPH_COL=$COL_WARN
      BAT_ICON=$BAT_ICON_LOW
    fi

    if [[ $BAT_STATE == "Charging," ]]; then
      BAT_ICON=$BAT_ICON_CHARGING
    fi

    BAT_GRAPH=$(echo $(( $BAT_PCT_REMAINING + 1 )) | dzen2-gdbar -h 20 -ss 1 -sw 4 -w 48 -s o -nonl -bg $COL_GRAPH_BORDER -fg $BAT_GRAPH_COL)
    echo "$SEPARATOR $BAT_ICON $BAT_TIME_REMAINING ($BAT_PCT_REMAINING%) $BAT_GRAPH " > $BAT_PIPE

    BAT_COUNTER=0
  else
    BAT_COUNTER=$(( $BAT_COUNTER + 1 ))
  fi

  sleep 1
done
