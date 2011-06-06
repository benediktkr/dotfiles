#!/bin/bash

# Load resources
xrdb -merge .Xresources

# Put trayer in the left corner (note, to use this, we must shrink xmobar's width
#trayer --edge top --align right --SetDockType true --SetPartialStrut true  --expand true --width 2 --transparent true --tint 0x191970 --height 12

xscreensaver -no-splash &
if [ -x /usr/bin/nm-applet] ; then
    nm-applet &  # --sm-disable
fi

exec xmonad