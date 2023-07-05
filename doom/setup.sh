#!/usr/bin/env bash

go get -u gitlab.com/shackra/goimapnotify
ln -s ~/.local/share/go/bin/goimapnotify ~/.local/bin/

xdg-mime default emacs.desktop text/org

update-mime-database ~/.local/share/mime
