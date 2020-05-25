#!/bin/sh

source "$HOME/.Xprofile"
xrdb "$HOME/.Xresources"

# exec /usr/bin/bspwm
exec /usr/bin/xmonad
