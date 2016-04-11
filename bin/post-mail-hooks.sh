#!/bin/sh

set -eu

# Index mail for searching
/usr/bin/notmuch new --quiet

# Index addresses for auto-completion
find $HOME/msg/ -type f -mmin -10 -print0 | \
  xargs -0 -n 1 -r /bin/sh -c 'lbdb-fetchaddr -a < "$1"' lbdb-fetchaddr
SORT_OUTPUT=name /usr/lib/lbdb/lbdb-munge
