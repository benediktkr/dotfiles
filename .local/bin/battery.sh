#!/bin/bash

SYS_AC="/sys/class/power_supply/AC/online"
SYS_ACAD="/sys/class/power_supply/ACAD/online"

BAT0=$(upower --show-info /org/freedesktop/UPower/devices/battery_BAT0 | grep percentage | awk '{ print $2 }')
BAT1=$(upower --show-info /org/freedesktop/UPower/devices/battery_BAT1 | grep percentage | awk '{ print $2 }')

if [[ -f "$SYS_AC" ]]; then
    AC=$(cat ${SYS_AC})
elif [[ -f "$SYS_ACAD" ]]; then
    AC=$(cat ${SYS_ACAD})
else
    AC="0"
fi


(
    echo '{'

    echo -ne '    "BAT0": "'
    echo -ne "${BAT0}"
    echo '",'

    echo -ne '    "BAT1": "'
    echo -ne "${BAT1}"
    echo '",'

    echo -ne '    "AC": '
    if [[ "$AC" == "1" ]]; then
        echo "true"
    else
        echo "false"
    fi
    echo '}'
) | jq .
