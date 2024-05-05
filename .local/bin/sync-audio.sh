#!/bin/bash

set -e

mkdir -pv /deadspace/audio/audiobooks
mkdir -pv /deadspace/audio/podcasts
mkdir -pv /deadspace/audio/music

echo "remote:"
ssh fsn-g0.vpn.sudo.is "du -sh /deadspace/audiobooks"
ssh fsn-g0.vpn.sudo.is "du -sh /deadspace/podcasts"
ssh fsn-g0.vpn.sudo.is "du -sh /deadspace/music"

echo "local:"
sudo du -sh /deadspace/audio/audiobooks
sudo du -sh /deadspace/audio/podcasts
sudo du -sh /deadspace/audio/music

echo "audiobooks"
sudo -E rsync -rah --numeric-ids --info=progress2 fsn-g0.vpn.sudo.is:/deadspace/audiobooks/ /deadspace/audio/audiobooks/

echo "podcasts"
sudo -E rsync -rah --numeric-ids --info=progress2 fsn-g0.vpn.sudo.is:/deadspace/podcasts/ /deadspace/audio/podcasts/

echo "music"
sudo -E rsync -rah --numeric-ids --info=progress2 fsn-g0.vpn.sudo.is:/deadspace/music/ /deadspace/audio/music/

