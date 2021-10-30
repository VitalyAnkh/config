set -x PATH /home/vitalyr/.opam/default/bin /home/vitalyr/sdk/lib/flutter/bin /home/vitalyr/sdk/app/jetbrains /home/vitalyr/.cargo/bin /home/vitalyr/.local/bin $HOME/.cabal/bin $HOME/.ghcup/bin $PATH /opt/anaconda/bin /opt/depot_tools
# set -x QT_QPA_PLATFORM wayland-egl
# set -x TERM xterm
# set -x http_proxy socks5://127.0.0.1:1080
# set -x https_proxy socks5://127.0.0.1:1080
# set -x all_proxy socks5://127.0.0.1:1080
set -x SDK_DIR $HOME/sdk
set -x PUB_HOSTED_URL https://pub.flutter-io.cn
set -x FLUTTER_STORAGE_BASE_URL https://storage.flutter-io.cn
set -x DOOMDIR $SDK_DIR/config/emacs/doom.d-vitalyr
set -x CHROME_EXECUTABLE google-chrome-stable
set -x WGPU_BACKEND vulkan
# erase the GDK_BACKEND variable
set -e GDK_BACKEND
set fish_greeting
# Set variables to let programs run natively under wayland
# This is firefox
# set -gx MOZ_ENABLE_WAYLAND 1
# This is LibreOffice
#export SAL_USE_VCLPLUGIN=gtk3
# For Qt5
# set -x QT_QPA_PLATFORM wayland-egl
# set -x QT_WAYLAND_DISABLE_WINDOWDECORATION 1
# set -x  QT_WAYLAND_FORCE_DPI physical
# For Elementary/EFL
# set -x ECORE_EVAS_ENGINE wayland_egl
# set ELM_ENGINE wayland_egl
# For SDL
# set -x  SDL_VIDEODRIVER wayland
# For Flatpak
# flatpak run --socket=wayland
# For GFLW
# Arch users should install `glfw-wayland`
# For Java under Xwayland
# set -x  _JAVA_AWT_WM_NONREPARENTING 1

# set -x  BEMENU_BACKEND wayland
# include config.d/*

zoxide init fish | source



mcfly init fish | source

# remove this when Nvidia driver support glutin and winit on Wayland fully
set -x WINIT_X10_BACKEND x11
