#!/bin/sh

set -eu

mkdir -p /tmp/dock-helper-pipes
rm -rf /tmp/dock-helper-pipes/*

WORKSPACES_AND_WINDOWS_PIPE="/tmp/dock-helper-pipes/workspaces-and-windows"
mkfifo $WORKSPACES_AND_WINDOWS_PIPE

NETWORK_PIPE="/tmp/dock-helper-pipes/network"
mkfifo $NETWORK_PIPE
~/bin/dock-helpers/network.sh &

VOLUME_PIPE="/tmp/dock-helper-pipes/volume"
mkfifo $VOLUME_PIPE
sleep 2
exec ~/bin/dock-helpers/volume-display.sh
