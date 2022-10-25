#!/bin/bash

if [[ ! -L /usr/lib/kitty/logo/kitty.png ]]; then
    cp -v /usr/lib/kitty/logo/kitty.png ./original/
fi
if [[ ! -L /usr/lib/kitty/logo/kitty-128.png ]]; then
    cp -v /usr/lib/kitty/logo/kitty-128.png ./original/
fi

mkdir -p ./kitty-icon/

echo "downloading icons from https://github.com/k0nserv/kitty-icon"
curl -s https://raw.githubusercontent.com/k0nserv/kitty-icon/main/icon_128x128.png > ./kitty-icon/kitty-128.png
curl -s https://raw.githubusercontent.com/k0nserv/kitty-icon/main/icon_32x32.png > ./kitty-icon/kitty.png

sudo rm -v /usr/lib/kitty/logo/kitty.png /usr/lib/kitty/logo/kitty-128.png
sudo ln -sv $(pwd)/kitty-icon/kitty.png /usr/lib/kitty/logo/kitty.png
sudo ln -sv $(pwd)/kitty-icon/kitty-128.png /usr/lib/kitty/logo/kitty-128.png
