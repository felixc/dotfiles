#!/bin/sh

set -eux

. $HOME/.keychain/*-sh

period="${1:-daily}"

dest_host="molniya.felixcrux.com"
dest_path="/depot/mir"

/usr/bin/ssh "$dest_host" /bin/rm -rf "$dest_path/$period-obsolete"

/usr/bin/nice --adjustment 15 /usr/bin/ionice --class 2 --classdata 5 \
  /usr/bin/rsync \
    --archive --acls --xattrs --copy-unsafe-links \
    --fuzzy --link-dest="../daily-1" \
    --delete-delay --ignore-errors \
    --exclude-from="$HOME/cfg/backup-exclude" \
    "$HOME/" "$dest_host:$dest_path/$period-in-progress"

/usr/bin/ssh "$dest_host" /bin/sh << EOF
  /bin/mv -f "$dest_path/$period-5" "$dest_path/$period-obsolete"
  /bin/mv "$dest_path/$period-4" "$dest_path/$period-5"
  /bin/mv "$dest_path/$period-3" "$dest_path/$period-4"
  /bin/mv "$dest_path/$period-2" "$dest_path/$period-3"
  /bin/mv "$dest_path/$period-1" "$dest_path/$period-2"
  /bin/mv "$dest_path/$period-in-progress" "$dest_path/$period-1"

  /bin/rm -rf "$dest_path/$period-obsolete"

  /bin/ln -sfn "$dest_path/$period-1" "$dest_path/latest"
EOF
