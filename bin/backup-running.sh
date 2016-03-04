#!/bin/sh

rsync -AXLay --delete --ignore-errors \
  --exclude .steam --exclude .cache --exclude wrk/git-annex/.git --exclude .cargo \
  $HOME/ /mnt/backup
