echo "Make dirs..."
mkdir -p ~/projects/web/hosts
mkdir -p ~/projects/rust
cd ~/projects && git clone git@github.com:VitalyAnkh/config.git

echo "Copy config files..."
cp ~/projects/config/.bashrc ~/
cp ~/projects/config/.zshrc ~/
cp ~/projects/config/.profile ~/

echo "Change shell to zsh"
chsh -s /bin/zsh

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
