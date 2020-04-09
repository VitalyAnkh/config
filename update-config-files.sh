# from now on, edit config files in config-archives only, don't edit config file in ~/ or /etc
# cp new config file when the one is newer

cp -u ~/.zshrc ~/sdk/config/config-archives/.zshrc
cp -u ~/.profile ~/sdk/config/config-archives/.profile
cp -u ~/.bashrc ~/sdk/config/config-archives/.bashrc
cp -u /etc/profile ~/sdk/config/config-archives/root-profile

cp -u ~/sdk/config/config-archives/.zshrc ~/.zshrc
cp -u ~/sdk/config/config-archives/.bashrc ~/.bashrc
cp -u ~/sdk/config/config-archives/.profile ~/.profile
sudo cp -u ~/sdk/config/config-archives/root-profile /etc/profile

cp ~/.zshrc ~/.config/
cp ~/.profile ~/.config/
cp ~/.bashrc ~/.config/
