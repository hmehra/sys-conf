#!/bin/bash

# remove existing files
rm -rf $HOME/.emacs       \
       $HOME/.tmux.conf   \
       $HOME/.myemacs     \
       $HOME/.emacsbackup

# Add files to respective locations
cp emacs $HOME/.emacs
cp tmux.conf $HOME/.tmux.conf
cp gitconfig $HOME/.gitconfig
cp -r myemacs $HOME/.myemacs

# make temp directory for emacs backup
(cd $HOME && mkdir .emacsbackup)
