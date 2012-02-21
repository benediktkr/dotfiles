#!/bin/sh

# Linker for my dotfiles

# Only laptop
if [ `hostname` = 'benedikt-laptop' ]; then
    ln -s $PWD/bashrc ~/.bashrc
fi


# Only desktop
if  [ `hostname` = 'turing' ]; then 
    echo "turing: xorg.conf"
    if [ -f "/etc/X11/xorg.conf" ]; then
        sudo mv /etc/X11/xorg.conf ~/xorg.conf-old
        echo "Note: Old xorg.conf backed up to ~/xorg.conf-old"
    fi
    sudo ln -s $PWD/xorg.conf-turing /etc/X11/xorg.conf
fi


# Computers with X (assume XMonad)
if [ `hostname` = 'benedikt-laptop' ] || [ `hostname` = 'turing' ]; then
    ln -s $PWD/xmonad.hs ~/.xmonad/xmonad.hs
    ln -s $PWD/xmobarrc-`hostname` ~/.xmobarrc

    ln -s $PWD/Xresources ~/.Xresources
    ln -s $PWD/xsession ~/.xsession
fi


ln -s $PWD/screenrc ~/.screenrc

# Makes alt act like meta. Originally for irssi in xterm.. I think. 
ln -s $PWD/inputrc ~/.inputrc

