#!/bin/bash

# remove existing files
rm -rf $HOME/.emacs       \
       $HOME/.tmux.conf   \
       $HOME/.emacsbackup

# Add files to respective locations
mkdir -p       $HOME/.emacs.d
cp emacs       $HOME/.emacs
cp myinit.el   $HOME/.emacs.d/
cp myinit.org  $HOME/.emacs.d/
cp tmux.conf   $HOME/.tmux.conf
cp gitconfig   $HOME/.gitconfig
cp bashrc_user $HOME/.bashrc_user

# make temp directory for emacs backup
(cd $HOME && mkdir .emacsbackup)

# Add custom bashrc
echo "source ~/.bashrc_user" >> ~/.bashrc
