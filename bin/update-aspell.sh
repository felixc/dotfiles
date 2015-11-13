#!/bin/sh

sorted_words=$(tail -n +2 $HOME/.aspell.en.pws | sort); echo "personal_ws-1.1 en" $(echo "$sorted_words" | wc -l)"\n$sorted_words" > $HOME/.aspell.en.pws
