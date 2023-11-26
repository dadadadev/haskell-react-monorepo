#!/bin/bash

sudo apt-get update
sudo apt-get install -y --no-install-recommends dpkg-dev gcc libc6-dev libffi-dev libgmp-dev libnuma-dev libtinfo-dev zlib1g-dev

# Configure HLS for Development in Root Directory
sudo rm -rf .stack-work
sudo rm -f stack.yaml stack.yaml.lock
stack init --force --resolver=lts-21.21 && stack build

stack install ghcid

cd server && sudo apt-get install -y sqlite3
sqlite3 db.sqlite3 "CREATE TABLE IF NOT EXISTS post (id INTEGER PRIMARY KEY, str text);"