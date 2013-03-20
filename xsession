#!/bin/bash

# Load resources and Xmodmap (assumes that files have been linked)
xrdb -merge ~/.Xresources
xmodmap ~/.Xmodmap


HITHER=repos/dotfiles

# Put trayer in the left corner
#trayer --edge top --align right --SetDockType true --SetPartialStrut true  \
#  --expand true --width 2 --transparent true --tint 0x191970 --height

# Set the background. 
WALLPAPER=chamberofunderstanding.jpg
if hash feh 2>&- && [ -f "$HITHER/$WALLPAPER" ]; then
    feh --bg-fill $HITHER/$WALLPAPER --no-fehbg
fi

if hash xscreensaver 2>&-; then 
    xscreensaver -no-splash &
fi

if hash nm-applet 2>&-; then
    nm-applet &
fi

if hash dropbox 2>&-; then
    dropbox start &
fi
    

# Host-dependent settings
if [ `hostname` = 'mathom' ]; then
    # ATI 5750
    #xrandr --output DVI-0 --left-of DVI-1 --output DVI-1 --auto
    # Onboard
    #xrandr --output VGA-0 --right-of HDMI-0 --auto
    # GeForece 8800 GTX with `nvidia` driver is done in xorg.conf which has been
    # given permission to live in this repo, so no xrandr currently.
    # GeForce 260GTX
    xrandr --output DVI-I-1 --left-of DVI-I-2 --auto
    blank_time=10
    xset s off
    xset -dpms

elif [ `hostname` = 'benedikt-laptop' ]; then
      blank_time=5
fi

xset b off
# I sometimes turn DPMS off and forget to turn it back on. 
xset +dpms  
xset s $[$blank_time*60]


eval `ssh-agent -s`

exec xmonad
