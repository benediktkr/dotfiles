#!/bin/bash

# Works on amine (Framework laptop running Fedora)

if [[ "$EUID" != "0" ]]; then
    echo "Must run as root"
    echo "Aborting."
    exit 1
fi

export DISPLAY=:0
xset dpms force off
