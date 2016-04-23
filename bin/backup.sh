#!/bin/sh

set -eu

period="${1:-daily}"

dest="/mnt/backup"

/bin/mv $dest/$period-6 $dest/$period-obsolete
/bin/mv $dest/$period-5 $dest/$period-6
/bin/mv $dest/$period-4 $dest/$period-5
/bin/mv $dest/$period-3 $dest/$period-4
/bin/mv $dest/$period-2 $dest/$period-3
/bin/mv $dest/$period-1 $dest/$period-2

/usr/bin/rsync \
  --archive --acls --xattrs --copy-unsafe-links \
  --fuzzy --link-dest=$dest/latest \
  --delete-delay --ignore-errors \
  --exclude-from=$HOME/cfg/backup-exclude \
  $HOME/ $dest/$period-1

/bin/ln -fsT $dest/$period-1 $dest/latest
/bin/rm -rf $dest/$period-obsolete
