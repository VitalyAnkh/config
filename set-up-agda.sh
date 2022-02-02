#!/usr/bin/env bash
## install softwares needed
## sudo pacman -S cabal cabal ghc git emacs
## or
## sudo apt-get install cabal ghc git emacs

mkdir -p ~/sdk
export SDK_DIR=~/sdk
export AGDA_SDK_PATH=$SDK_DIR/lib/agda # replace with your own
mkdir -p $AGDA_SDK_PATH

## installing agda
cabal update
cabal install alex happy cpphs
cabal install --allow-newer Agda
## do these things to your emacs config files
##agda-mode setup
##agda-mode compile
git clone https://github.com/agda/agda-stdlib.git $AGDA_SDK_PATH/agda-stdlib/
rm $AGDA_SDK_PATH/agda-stdlib/src/index.agda
mkdir ~/.agda

echo "${AGDA_SDK_PATH}/agda-stdlib/standard-library.agda-lib" >> ~/.agda/libraries
echo "standard-library" >> ~/.agda/defaults

echo "Please check whether the content of files is right"
echo "$HOME/.agda/libraries is"
cat $HOME/.agda/libraries

echo "Done!"
echo "Remember to pull the latest commits from the agda remote repo!"
