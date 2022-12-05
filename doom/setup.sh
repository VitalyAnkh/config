#!/usr/bin/env bash

systemctl --user disable emacs.service

xdg-mime default emacs.desktop text/org

texdef -t pdflatex -p graphicx Gin@extensions

go get -u gitlab.com/shackra/goimapnotify
ln -s ~/.local/share/go/bin/goimapnotify ~/.local/bin/

export GSASL_VERSION=1.8.1
cd ~/.local/lib/
curl "ftp://ftp.gnu.org/gnu/gsasl/libgsasl-$GSASL_VERSION.tar.gz" | tar xz
curl "ftp://ftp.gnu.org/gnu/gsasl/gsasl-$GSASL_VERSION.tar.gz" | tar xz
cd "./libgsasl-$GSASL_VERSION"
./configure
make
sudo make install
cd ..
cd "./gsasl-$VERSION"
./configure
make
sudo make install
cd ..

cd ~/.local/lib/
git clone https://github.com/marlam/msmtp-mirror.git ./msmtp
cd ./msmtp
libtoolize --force
aclocal
autoheader
automake --force-missing --add-missing
autoconf
# if using GSASL
# PKG_CONFIG_PATH=/usr/local/lib/pkgconfig ./configure --with-libgsasl
./configure
make
sudo make install

update-desktop-database ~/.local/share/applications
