echo "Copy config files..."
cp ~/projects/config/.bashrc ~/
cp ~/projects/config/.zshrc ~/
cp ~/projects/config/.profile ~/

echo "Change shell to zsh"
chsh -s /bin/zsh 

echo "Configure directory jump..."
mkdir /home/vitalyr/projects
mkdir /home/vitalyr/projects/shell 
git clone git@github.com:rupa/z.git
./home/projects/shell/z/z.sh 

echo "Configure rust environment..."
curl https://sh.rustup.rs -sSf | sh
