# Script to print supported TMUX colors in bash

for i in {0..255} ; do
    printf "\x1b[38;5;${i}mcolour${i}\n"
    sleep 0.5
done
