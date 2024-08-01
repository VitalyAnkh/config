#!/usr/bin/env bash

cd /tmp
if [ ! -d hunspell-en-custom ]; then
    curl -o "hunspell-en-custom.zip" 'http://app.aspell.net/create?max_size=80&spelling=GBs&spelling=AU&max_variant=0&diacritic=keep&special=hacker&special=roman-numerals&encoding=utf-8&format=inline&download=hunspell'
    unzip "hunspell-en-custom.zip" -d hunspell-en-custom
fi

cd hunspell-en-custom
DESTDIR1="$HOME/.local/share/hunspell"
DESTDIR2="$HOME/.config/enchant/hunspell"
mkdir -p "$DESTDIR1"
mkdir -p "$DESTDIR2"
cp en-custom.{aff,dic} "$DESTDIR1"
cp en-custom.{aff,dic} "$DESTDIR2"

cd /tmp
if [ ! -d aspell6-en-custom ]; then
    curl -o "aspell6-en-custom.tar.bz2" 'http://app.aspell.net/create?max_size=80&spelling=GBs&spelling=AU&max_variant=0&diacritic=keep&special=hacker&special=roman-numerals&encoding=utf-8&format=inline&download=aspell'
    tar -xjf "aspell6-en-custom.tar.bz2"
fi

cd aspell6-en-custom
DESTDIR="$HOME/.config/enchant/" ./configure
sed -i 's/dictdir = .*/dictdir = "aspell"/' Makefile
sed -i 's/datadir = .*/datadir = "aspell"/' Makefile
make && make install

go get -u gitlab.com/shackra/goimapnotify
ln -s ~/.local/share/go/bin/goimapnotify ~/.local/bin/

update-desktop-database ~/.local/share/applications

xdg-mime default emacs.desktop text/org
