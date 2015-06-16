#!/bin/bash

# Load resources and Xmodmap (assumes that files have been linked)
xrdb -merge ~/.Xresources
#xmodmap ~/.Xmodmap

BASE_DIR=/home/benedikt/projects/dotfiles

# Put trayer in the left corner
#trayer --edge top --align right --SetDockType true --SetPartialStrut true  \
#  --expand true --width 2 --transparent true --tint 0x191970 --height

# Set the background. 
WALLPAPER=chamberofunderstanding.jpg
if hash feh 2>&- && [ -f "$BASE_DIR/bgpics/$WALLPAPER" ]; then
    feh --bg-fill $BASE_DIR/bgpics/$WALLPAPER --no-fehbg
fi

if hash light-locker 2>&-; then 
    light-locker --lock-on-suspend &
fi

if hash nm-applet 2>&-; then
    nm-applet &
fi

# Host-dependent settings
if [ `hostname` = 'burrow' ]; then
      blank_time=5
fi

xset b off
# I sometimes turn DPMS off and forget to turn it back on. 
xset +dpms  
xset s $[$blank_time*60]

eval `ssh-agent -s`
eval `gpg-agent --daemon --enable-ssh-support --write-env-file "${HOME}/.gpg-agent-info"`

exec xmonad
