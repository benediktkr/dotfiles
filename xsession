#!/bin/bash

# Load resources
xrdb -merge .Xresources

# Put trayer in the left corner
#trayer --edge top --align right --SetDockType true --SetPartialStrut true  \
#  --expand true --width 2 --transparent true --tint 0x191970 --height

# Set the background. In ubuntu `xpmroot` is provided by 'fvwm'.
xpmroot ~/bg.jpg &

xscreensaver -no-splash &
if [ -x "/usr/bin/nm-applet" ]; then
	nm-applet &
fi

exec xmonad
