set -x PATH /home/vitalyr/.opam/default/bin /home/vitalyr/sdk/lib/flutter/bin /home/vitalyr/sdk/app/jetbrains /home/vitalyr/.cargo/bin /home/vitalyr/.local/bin $HOME/.cabal/bin $HOME/.ghcup/bin $PATH /opt/anaconda/bin /opt/depot_tools
# set -x TERM xterm
# set -x http_proxy socks5://127.0.0.1:1080
# set -x https_proxy socks5://127.0.0.1:1080
# set -x all_proxy socks5://127.0.0.1:1080
set -x SDK_DIR $HOME/sdk
set -x PUB_HOSTED_URL https://pub.flutter-io.cn
set -x FLUTTER_STORAGE_BASE_URL https://storage.flutter-io.cn
set -x DOOMDIR $HOME/.config/doom
set -x CHROME_EXECUTABLE google-chrome-stable
set -x WGPU_BACKEND vulkan
set -x GUAKE_ENABLE_WAYLAND 1
# erase the GDK_BACKEND variable
set -e GDK_BACKEND
set fish_greeting

# include config.d/*

zoxide init fish | source
mcfly init fish | source

eval (opam env)
set -gx WASMTIME_HOME "$HOME/.wasmtime"

string match -r ".wasmtime" "$PATH" > /dev/null; or set -gx PATH "$WASMTIME_HOME/bin" $PATH