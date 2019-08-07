#!/bin/bash

# remove existing files
rm -rf $HOME/.emacs       \
       $HOME/.tmux.conf   \
       $HOME/.myemacs     \
       $HOME/.emacsbackup

# Add files to respective locations
mkdir -p $HOME/.emacs.d
cp myinit.el $HOME/.emacs.d
cp myinit.org $HOME/.emacs.d
cp tmux.conf $HOME/.tmux.conf
cp gitconfig $HOME/.gitconfig
cp -r myemacs $HOME/.myemacs

# make temp directory for emacs backup
(cd $HOME && mkdir .emacsbackup)
