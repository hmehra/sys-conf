#!/bin/bash

# remove existing files
rm $HOME/.emacs $HOME/.gitconfig $HOME/.ssh/config $HOME/.tmux.conf
(cd $HOME && rm -rf .emacsbackup)

# Add files to respective locations
cp emacs $HOME/.emacs
cp ssh.config $HOME/.ssh/config
cp tmux.conf $HOME/.tmux.conf
cp gitconfig $HOME/.gitconfig
cp -r myemacs/ $HOME/.myemacs

# make temp directory for emacs backup
(cd $HOME && mkdir .emacsbackup)