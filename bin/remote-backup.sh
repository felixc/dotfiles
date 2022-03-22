#!/bin/sh

set -eux

. "$HOME"/.keychain/*-sh

period="${1:-daily}"
case "$period" in
    "daily" )
        backups_to_keep=7 ;;
    "weekly" )
        backups_to_keep=4 ;;
    "monthly" )
        backups_to_keep=12 ;;
    * )
        print "Unrecognized backup time period." 1>&2
        exit 1 ;;
esac

dest_host="molniya.felixcrux.com"
dest_path="/depot/mir"

/usr/bin/ssh "$dest_host" /bin/rm -rf "$dest_path/$period-obsolete"

/usr/bin/nice --adjustment 15 /usr/bin/ionice --class 2 --classdata 5 \
  /usr/bin/rsync \
    --archive --acls --xattrs --copy-unsafe-links --hard-links \
    --fuzzy --link-dest="../daily-1" \
    --delete-delay --ignore-errors \
    --exclude-from="$HOME/cfg/backup-exclude" \
    "$HOME/" "$dest_host:$dest_path/$period-in-progress"

/usr/bin/ssh "$dest_host" /bin/sh << EOF
  set -ux
  /bin/mv -f "$dest_path/$period-$backups_to_keep" "$dest_path/$period-obsolete"
  backups_to_keep=$backups_to_keep  # Set the var remotely so we can use it in math
  offset=1
  while [ \$offset -lt \$backups_to_keep ]; do
    /bin/mv "$dest_path/$period-\$((backups_to_keep - offset))" \
            "$dest_path/$period-\$((backups_to_keep - offset + 1))"
    offset=\$((offset + 1))
  done
  /bin/mv "$dest_path/$period-in-progress" "$dest_path/$period-1"

  /bin/rm -rf "$dest_path/$period-obsolete"

  /bin/ln -sfn "$dest_path/$period-1" "$dest_path/latest"
EOF
