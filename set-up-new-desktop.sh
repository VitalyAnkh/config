#!/usr/bin/env bash
# TODO: use ansible to set up

export SDK_DIR="${HOME}/sdk"
echo "Make dirs..."
mkdir -p ~/projects/web/hosts
mkdir -p ~/projects/rust
cd ~/projects && git clone git@github.com:VitalyAnkh/config.git

# add mirror site for conda
pip config set global.index-url https://pypi.tuna.tsinghua.edu.cn/simple

echo "Setting up rbenv..."
git clone https://github.com/rbenv/rbenv.git ~/.rbenv
cd ~/.rbenv && src/configure && make -C src
echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >>~/.zshrc
~/.rbenv/bin/rbenv init
#echo "********************************************************"
#echo "Setting up gvm..."
#cd ~/projects/shell
#git clone git@github.com:moovweb/gvm.git
#cd gvm/binscript
#sh gvm-installer
#source ~/.gvm/scripts/gvm
echo "*******************************************************"
echo "Settup haskell tools"
sudo pacman -S hie hindent stylish-haskell

echo "Setting up ruby-build..."
mkdir -p ~/projects/ruby && cd ~/projects/ruby && git clone https://github.com/rbenv/ruby-build.git
PREFIX=/usr/local ./ruby-build/install.sh

# echo "Setting up haskell env..."
# mkdir -p ~/projects/haskell && cd ~/projects/haskell && git clone https://github.com/haskell/haskell-ide-engine --recursive
# make build-all

echo "Copy config files..."
cp ~/projects/config/.bashrc ~/
cp ~/projects/config/.zshrc ~/
cp ~/projects/config/.profile ~/

echo "Change shell to zsh"
chsh -s /bin/zsh

echo "Set up IJava"
cd ~/projects/java && git clone https://github.com/SpencerPark/IJava.git && cd IJava
chmod u+x gradlew && ./gradlew installKernel

echo "Configure directory jump..."
mkdir -p /home/vitalyr/projects/shell
git clone git@github.com:rupa/z.git
chmod u+x ~/projects/shell/z/z.sh
./home/projects/shell/z/z.sh

echo "Config hosts"
cd ~/projects/web && git clone git@github.com:googlehosts/hosts.git
cp hosts/hosts-files/* /etc/*
sudo chmod o+w /etc/hosts
sudo echo "
52.84.246.90    d3c33hcgiwev3.cloudfront.net
52.84.246.252    d3c33hcgiwev3.cloudfront.net
52.84.246.144    d3c33hcgiwev3.cloudfront.net
52.84.246.72    d3c33hcgiwev3.cloudfront.net
52.84.246.106    d3c33hcgiwev3.cloudfront.net
52.84.246.135    d3c33hcgiwev3.cloudfront.net
52.84.246.114    d3c33hcgiwev3.cloudfront.net
52.84.246.90    d3c33hcgiwev3.cloudfront.net
52.84.246.227    d3c33hcgiwev3.cloudfront.net
" >>/etc/hosts

echo "Configure rust environment..."
curl https://sh.rustup.rs -sSf | sh

source $HOME/.cargo/env

curl -L git.io/antigen >antigen.zsh

jupyter labextension install jupyterlab_nbmetadata

echo "Install rust-analyzer..."
git clone https://github.com/rust-analyzer/rust-analyzer.git --depth 1
cd rust-analyzer
cargo install-code
# rustup component add rust-src # no longer need

cargo install lsd
cargo install bat
