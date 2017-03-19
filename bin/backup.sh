#!/bin/sh

set -u

period="${1:-daily}"

dest="/mnt/backup"

/bin/mountpoint -q "$dest" ||
  { echo "Error: $dest is not mounted" >&2; exit 1; }

/usr/bin/nice --adjustment 19 /usr/bin/ionice --class 2 --classdata 7 \
  /usr/bin/rsync \
    --archive --acls --xattrs --copy-unsafe-links \
    --fuzzy --link-dest="$dest/$period-2" \
    --delete-delay --ignore-errors \
    --exclude-from="$HOME/cfg/backup-exclude" \
    "$HOME/" "$dest/$period-in-progress"

/bin/mv -f "$dest/$period-6" "$dest/$period-obsolete"
/bin/mv "$dest/$period-5" "$dest/$period-6"
/bin/mv "$dest/$period-4" "$dest/$period-5"
/bin/mv "$dest/$period-3" "$dest/$period-4"
/bin/mv "$dest/$period-2" "$dest/$period-3"
/bin/mv "$dest/$period-1" "$dest/$period-2"
/bin/mv "$dest/$period-in-progress" "$dest/$period-1"

/bin/ln -fsT "$dest/$period-1" "$dest/latest"

/usr/bin/nice --adjustment 19 /usr/bin/ionice --class 2 --classdata 7 \
  /bin/rm -rf "$dest/$period-obsolete"
