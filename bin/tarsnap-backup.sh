#!/bin/sh

/usr/bin/tarsnap -c \
  -f "mir-$(/bin/date +%F)" \
  -s "|\(.*\)|mir-$(/bin/date +%F)/\1|" \
  -C /depot/mir/latest \
  old usr wrk
