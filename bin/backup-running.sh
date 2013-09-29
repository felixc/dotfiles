#!/bin/sh

. $HOME/.keychain/mir-sh

rsync -Lazy --delete-after $HOME/snd $HOME/img $HOME/box $HOME/old $HOME/wrk \
  molniya:~/backup/mir-running
