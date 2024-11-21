#!/bin/bash

for item in settings.json keybindings.json; do
    echo "- ${item}"
    cp "$HOME/Library/Application Support/Code/User/settings.json" $HOME/.config/Code/User/
done

yadm st -- .config/Code
