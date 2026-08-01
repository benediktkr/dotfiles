#!/usr/bin/env bash

# https://nikitabobko.github.io/AeroSpace/goodies#show-aerospace-workspaces-in-sketchybar

if [ "$1" = "$FOCUSED_WORKSPACE" ]; then
    sketchybar --set $NAME background.drawing=on
else
    sketchybar --set $NAME background.drawing=off
fi
