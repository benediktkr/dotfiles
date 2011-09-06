#!/bin/sh

# Linker for my dotfiles

if [ `hostname` = 'benedikt-laptop' ]; then
    ln -s $PWD/bashrc ~/.bashrc
fi

ln -s $PWD/xmonad.hs ~/.xmonad/xmonad.hs
ln -s $PWD/xmobarrc-`hostname` ~/.xmobarrc

ln -s $PWD/screenrc ~/.screenrc

ln -s $PWD/Xresources ~/.Xresources
ln -s $PWD/xsession ~/.xsession

# Make alt act like meta. Originally for irssi in xterm.. I think. 
ln -s $PWD/inputrc ~/.inputrc

