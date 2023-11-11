#!/bin/bash
#

# https://www.reddit.com/r/archlinux/comments/mjlblj/bluetooth_keyboard_laggy_unless_bluetooth/
# https://askubuntu.com/questions/1264725/how-to-disable-bluetooth-power-saving

if [[ "$EUID" == "0" ]]; then
    echo -1 > /sys/module/usbcore/parameters/autosuspend
    grep -H . /sys/module/usbcore/parameters/autosuspend
else
    echo "Invoking '$(basename $0)' with sudo:"
    echo " + sudo $0"
    sudo $0
fi

