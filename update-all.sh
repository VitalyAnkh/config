echo "Copy config files..."
cp ~/.zshrc ~/.config/
cp ~/.profile ~/.config/
cp ~/.bashrc ~/.config/
cp ~/.bashrc ~/projects/config
cp ~/.zshrc ~/projects/config
cp ~/.profile ~/projects/config
echo "Exchange CapsLock and Ctrl in X11..."
setxkbmap -option "ctrl:swapcaps"
echo "modprobe bluetooth..."
modprobe bluetooth
echo "modprobe nvidia..."
sudo modprobe nvidia
echo "Navigat GPU status..."
nvidia-smi
echo "Update flutter..."
flutter upgrade
echo "Update rust..."
rustup update
echo "Update cargo components..."
cargo install-update -a
echo "Update anaconda libraries..."
conda update --all -y
echo "Update haskell stack libraries..."
stack update
echo "Update OCaml libraries..."
opam update
opam upgrade -y
echo "Update system packages..."
yay
echo "Done!"
