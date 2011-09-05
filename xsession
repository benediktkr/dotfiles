#!/bin/bash

# Load resources
xrdb -merge .Xresources

# Put trayer in the left corner
#trayer --edge top --align right --SetDockType true --SetPartialStrut true  \
#  --expand true --width 2 --transparent true --tint 0x191970 --height

# Set the background. In ubuntu `xpmroot` is provided by 'fvwm'. Seemingly does not work
if [ -x "/usr/bin/xpmroot" ]; then
    xpmroot ~/repos/dotfiles/bg.jpg &
fi

xscreensaver -no-splash &

if [ `hostname` = 'turing' ]; then
    xrandr --output DVI-0 --left-of DVI-1 --output DVI-1 --auto
elif [ `hostname` = 'benedikt-laptop' ]; then
    nm-applet &
fi

eval `ssh-agent -s`

exec xmonad
