#!/bin/sh

set -eux

. "$HOME"/.keychain/*-sh

dest_path="/depot/pool/music-collection-backup"

rm -rf "$dest_path"/in-progress

/usr/bin/nice --adjustment 15 /usr/bin/ionice --class 2 --classdata 5 \
  /usr/bin/rsync \
    --archive --acls --xattrs --copy-unsafe-links --hard-links \
    --fuzzy --link-dest="../weekly-1" \
    --delete-delay --ignore-errors \
    molniya.felixcrux.com:/depot/music-collection/ "$dest_path"/in-progress


set -ux

backups_to_keep=12

/bin/mv -f "$dest_path"/weekly-12 "$dest_path"/obsolete
offset=1
while [ $offset -lt $backups_to_keep ]; do
  /bin/mv "$dest_path/weekly-$((backups_to_keep - offset))" \
          "$dest_path/weekly-$((backups_to_keep - offset + 1))"
  offset=$((offset + 1))
done
/bin/mv "$dest_path"/in-progress "$dest_path"/weekly-1

/bin/rm -rf "$dest_path"/obsolete
