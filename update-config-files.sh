# from now on, edit config files in config-archives only, don't edit config file in ~/ or /etc
# cp new config file when the one is newer
cp -u ~/projects/config/config-archives/.zshrc ~/.zshrc
cp -u ~/projects/config/config-archives/.bashrc ~/.bashrc
cp -u ~/projects/config/config-archives/.profile ~/.profile
sudo cp -u ~/projects/config/config-archives/root-profile /etc/profile

cp -u ~/.zshrc ~/projects/config/config-archives/.zshrc
cp -u ~/.profile ~/projects/config/config-archives/.profile
cp -u ~/.bashrc ~/projects/config/config-archives/.bashrc
cp -u /etc/profile ~/projects/config/config-archives/root-profile

cp ~/.zshrc ~/.config/
cp ~/.profile ~/.config/
cp ~/.bashrc ~/.config/
