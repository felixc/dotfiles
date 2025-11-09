#!/bin/sh

set -eu

NETWORK_PIPE="/tmp/dock-helper-pipes/network"

REFRESH_INTERVAL_SECS=15

RX_BYTES_OLD=0
TX_BYTES_OLD=0

while true; do
  ACTIVE_INTERFACE=$(route -n | grep -E " UGH? " | awk 'END {print $8}')
  IP_ADDR=$(ip addr show "$ACTIVE_INTERFACE" scope global | sed -ne 's/.*inet \([^ ]*\)\/.*/\1/p')

  TX_BYTES=$(cat /sys/class/net/$ACTIVE_INTERFACE/statistics/tx_bytes)
  RX_BYTES=$(cat /sys/class/net/$ACTIVE_INTERFACE/statistics/rx_bytes)
  RX_RATE=$(( (RX_BYTES - RX_BYTES_OLD) / 1024 / REFRESH_INTERVAL_SECS ))
  TX_RATE=$(( (TX_BYTES - TX_BYTES_OLD) / 1024 / REFRESH_INTERVAL_SECS ))
  if [ $RX_RATE -gt 999 ]; then
    RX_RATE=$(echo "scale=1; $RX_RATE / 1024" | bc)" MB/s"
  else
    RX_RATE="${RX_RATE} kB/s"
  fi
  if [ $TX_RATE -gt 999 ]; then
    TX_RATE=$(echo "scale=1; $TX_RATE / 1024" | bc)" MB/s"
  else
    TX_RATE="${TX_RATE} kB/s"
  fi
  RX_BYTES_OLD=$RX_BYTES
  TX_BYTES_OLD=$TX_BYTES

  echo "{ \"interfaces\": [ { \"name\": \"$ACTIVE_INTERFACE\", \"ip\": \"$IP_ADDR\", \"rx\": \"$RX_RATE\", \"tx\": \"$TX_RATE\" } ] }" \
    > "$NETWORK_PIPE"
  sleep $REFRESH_INTERVAL_SECS
done
