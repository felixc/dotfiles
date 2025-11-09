#!/bin/sh

set -eu

VOLUME_PIPE="/tmp/dock-helper-pipes/volume"

VOLUME=$(pamixer --get-volume)
IS_MUTED=$(pamixer --get-mute)

echo "{ \"value\": $VOLUME, \"is_muted\": $IS_MUTED }" > "$VOLUME_PIPE"
