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
    blank_time = 10
elif [ `hostname` = 'benedikt-laptop' ]; then
    nm-applet &
    blank_time = 5
fi

xset b off
# I sometimes turn DPMS off and forget to turn it back on. 
xset +dpms  
xset s $[$blank_time*60]


eval `ssh-agent -s`

exec xmonad
