set -x PATH /home/vitalyr/.opam/default/bin /home/vitalyr/sdk/lib/flutter/bin /home/vitalyr/sdk/app/jetbrains /home/vitalyr/.cargo/bin /home/vitalyr/.local/bin $HOME/.cabal/bin $HOME/.ghcup/bin $PATH /opt/anaconda/bin /opt/depot_tools
# set -x TERM xterm
# set -x http_proxy socks5://127.0.0.1:1080
# set -x https_proxy socks5://127.0.0.1:1080
# set -x all_proxy socks5://127.0.0.1:1080
set -x SDK_DIR $HOME/sdk
set -x PUB_HOSTED_URL https://pub.flutter-io.cn
set -x FLUTTER_STORAGE_BASE_URL https://storage.flutter-io.cn
# set -x DOOMDIR $HOME/.config/emacs/doom.d-vitalyr
set -x CHROME_EXECUTABLE google-chrome-stable
set -x WGPU_BACKEND vulkan
# erase the GDK_BACKEND variable
set -e GDK_BACKEND
set fish_greeting

# include config.d/*

zoxide init fish | source
mcfly init fish | source

# remove this when Nvidia driver support glutin and winit on Wayland fully
set -x WINIT_UNIX_BACKEND x11
eval (opam env)