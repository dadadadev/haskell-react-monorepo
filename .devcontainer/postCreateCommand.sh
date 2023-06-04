#!/bin/bash

sudo apt-get update
sudo apt-get install -y --no-install-recommends dpkg-dev gcc libc6-dev libffi-dev libgmp-dev libnuma-dev libtinfo-dev zlib1g-dev

# enable HLS to load stack projects in subdirectories
stack init --force && stack build