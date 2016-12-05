#!/bin/bash

sudo rm -rf data
mkdir data

echo -e "\nSaving installed packages list"
dpkg --get-selections >| ./data/package.list
echo -e "Done\n"

echo -e "Copying all old sources"
sudo cp -R /etc/apt/sources.list* ./data/
echo -e "Done\n"

echo -e "Exporting all old keys"
sudo apt-key exportall >| ./data/Repo.keys
echo -e "Done\n"

echo -e "Adding permissions on old sources and removing backups"
sudo chown -R `whoami` data/

find data/ -iname *backup -exec rm -f {} \;
find data/ -iname *.save -exec rm -f {} \;
find data/ -iname *~ -exec rm -f {} \;
find data/ -iname *distUpgrade -exec rm -f {} \;
echo -e "Done\n"

exit 0
