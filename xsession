#!/bin/bash

# Load resources
xrdb -merge .Xresources

HITHER=repos/dotfiles

# Put trayer in the left corner
#trayer --edge top --align right --SetDockType true --SetPartialStrut true  \
#  --expand true --width 2 --transparent true --tint 0x191970 --height

# Set the background. 
WALLPAPER=milkyway.jpg
if hash feh 2>&- && [ -f "$HITHER/$WALLPAPER" ]; then
    feh --bg-fill $HITHER/$WALLPAPER --no-fehbg
fi

if hash xscreensaver 2>&-; then 
    xscreensaver -no-splash &
fi

if hash nm-applet 2>&-; then
    nm-applet &
fi

# Host-dependent settings
if [ `hostname` = 'turing' ]; then
    # ATI 5750
    #xrandr --output DVI-0 --left-of DVI-1 --output DVI-1 --auto
    # Onboard
    #xrandr --output VGA-0 --right-of HDMI-0 --auto
    # GeForece 8800 GTX with `nvidia` driver is done in xorg.conf which has been
    # given permission to live in this repo, so no xrandr currently. 
    blank_time=10

elif [ `hostname` = 'benedikt-laptop' ]; then
      blank_time=5
fi

xset b off
# I sometimes turn DPMS off and forget to turn it back on. 
xset +dpms  
xset s $[$blank_time*60]


eval `ssh-agent -s`

exec xmonad
