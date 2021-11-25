#!/usr/bin/env bash

xdg-mime default emacs.desktop text/org

go get -u gitlab.com/shackra/goimapnotify
ln -s ~/.local/share/go/bin/goimapnotify ~/.local/bin/

update-desktop-database ~/.local/share/applications
