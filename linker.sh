#!/bin/sh

# Linker for my dotfiles

if [ `hostname` = 'benedikt-laptop' ]; then
    ln -s $PWD/bashrc ~/.bashrc
fi

# Computers with X (assume XMonad)
if [ `hostname` = 'benedikt-laptop' ] || [ `hostname` = 'turing' ]; then
    echo "X and XMonad assumed"
    ln -s $PWD/xmonad.hs ~/.xmonad/xmonad.hs
    ln -s $PWD/xmobarrc-`hostname` ~/.xmobarrc

    ln -s $PWD/Xresources ~/.Xresources
    ln -s $PWD/xsession ~/.xsession
fi

ln -s $PWD/screenrc ~/.screenrc

# Makes alt act like meta. Originally for irssi in xterm.. I think. 
ln -s $PWD/inputrc ~/.inputrc

