# TODO: use ansible to update
echo "Copy config files..."
cp ~/.zshrc ~/.config/
cp ~/.profile ~/.config/
cp ~/.bashrc ~/.config/
cp ~/.bashrc ~/projects/config
cp ~/.zshrc ~/projects/config
cp ~/.profile ~/projects/config
echo "Update hosts files..."
cd ~/projects/web/hosts/
git pull
echo "Copy hosts files..."
sudo cp hosts-files/* /etc/
echo "Add hosts to support coursera..."
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
echo "Exchange CapsLock and Ctrl in X11..."
setxkbmap -option "ctrl:swapcaps"
echo "modprobe bluetooth..."
modprobe bluetooth
echo "modprobe nvidia..."
sudo modprobe nvidia
echo "Update npm..."
npm install npm@latest -g
echo "Update rbenv..."
cd ~/.rbenv
git pull
echo "Update ruby build..."
cd "$(rbenv root)"/plugins/ruby-build && git pull
echo "Navigat GPU status..."
nvidia-smi
echo "Update flutter..."
flutter upgrade
echo "Update rust..."
rustup update
echo "Update cargo components..."
cargo install-update -a
echo "Update rust analyzer..."
cd ~/projects/rust/rust-analyzer
git checkout -- Cargo.lock
git pull
cargo install-code
rustup component add rust-src
# echo "Update anaconda libraries..."
# conda update --all -y
echo "Update haskell stack libraries..."
stack update
echo "Update OCaml libraries..."
opam update
opam upgrade -y
echo "Update system packages..."
yay
echo "Done!"
