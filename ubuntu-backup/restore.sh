#!/bin/bash

echo -e "\nAdding keys back"
sudo apt-key add data/Repo.keys
echo -e "Done\n"

echo -e "Adding sources back"
sudo cp -R data/sources.list* /etc/apt/
echo -e "Done\n"

echo -e "Restoring apps back"
sudo apt-get update
sudo apt-get -y install dselect
sudo dpkg --set-selections < data/package.list
sudo dselect
echo -e "Done restoring state\n"
