#!/bin/bash

# remove existing files
rm $HOME/.emacs $HOME/.tmux.conf
(cd $HOME && rm -rf .emacsbackup)

# Add files to respective locations
cp emacs $HOME/.emacs
cp tmux.conf $HOME/.tmux.conf
cp -r myemacs/ $HOME/.myemacs

# make temp directory for emacs backup
(cd $HOME && mkdir .emacsbackup)