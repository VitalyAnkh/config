#!/usr/bin/env bash

go get -u gitlab.com/shackra/goimapnotify
ln -s ~/.local/share/go/bin/goimapnotify ~/.local/bin/

~/.config/doom/misc/mbsync-imapnotify.py

systemctl --user enable mbsync.timer --now

update-desktop-database ~/.local/share/applications

xdg-mime default emacs.desktop text/org
