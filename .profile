# set XDG environment variables
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share

# for Doom Emacs
export DOOMDIR=$XDG_CONFIG_HOME/doom

export LSP_USE_PLISTS=true

export WINEPREFIX=$XDG_CONFIG_HOME/.wine

# for fcitx5
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export XIM=fcitx
export XIM_PROGRAM=fcitx
export SDL_IM_MODULE=fcitx
export CHROME_EXECUTABLE=google-chrome-stable

# let rust's cargo use sparse registry
export CARGO_UNSTABLE_SPARSE_REGISTRY=true

# for blender
export BLENDER_WAYLAND=1

# allow nix install proprietary packages
export NIXPKGS_ALLOW_UNFREE=1

# for golang tools
export GOPROXY=https://goproxy.io,direct

# make guake run under wayland
export GUAKE_ENABLE_WAYLAND=1

# for wlroots
# yeah nvidia 515.43 driver supports VK_EXT_drm_image_format_modifier
export WLR_RENDERER=vulkan
export WLR_NO_HARDWARE_CURSORS=1

# for nvidia proprietary driver
export __GLX_VENDOR_LIBRARY_NAME=nvidia
export GBM_BACKEND=nvidia-drm

# for video hardware acceleration on nvidia
export NVD_BACKEND=egl
export VDPAU_DRIVER=nvidia
export LIBVA_DRIVER_NAME=nvidia

# Set variables to let programs run natively under wayland
# for gtk3 applications
#export GDK_BACKEND=wayland

# This is firefox
export MOZ_ENABLE_WAYLAND=1
export MOZ_WEBRENDER=1
# This is LibreOffice
export SAL_USE_VCLPLUGIN=gtk3

# For Qt5
export QT_QPA_PLATFORM=wayland
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_AUTO_SCREEN_SCALE_FACTOR=0
#export QT_SCREEN_SCALE_FACTOR=1
#export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
#export QT_WAYLAND_FORCE_DPI=physical

# For Elementary/EFL
export ECORE_EVAS_ENGINE=wayland_egl
export ELM_ENGINE=wayland_egl
# For SDL
export SDL_VIDEODRIVER="wayland,x11"
# For Flatpak
# flatpak run --socket=wayland
# For GLFW
# Arch users should install `glfw-wayland`
# For Java under Xwayland
export _JAVA_AWT_WM_NONREPARENTING=1
export CLUTTER_BACKEND=wayland
export BEMENU_BACKEND=wayland
# Android configuration
# not OK. to be fixed
export ANDROID_SDK_ROOT=/home/vitalyr/Android/Sdk
export ANDROID_SDK_HOME=/home/vitalyr/Android/Sdk
export ANDROID_AVD_HOME=$ANDROID_SDK_ROOT/avd

export PATH="$PATH:/usr/bin:$HOME/sdk/lib/flutter/bin:$HOME/sdk/app/jetbrains:$HOME/.cargo/bin:$HOME/.local/bin:$HOME/.cabal/bin:$HOME/.ghcup/bin:$HOME/.local/share/coursier/bin"

export SDK_DIR=$HOME/sdk

# for flutter
export PUB_HOSTED_URL=https://pub.flutter-io.cn
export FLUTTER_STORAGE_BASE_URL=https://storage.flutter-io.cn

export WGPU_BACKEND=vulkan
export GUAKE_ENABLE_WAYLAND=1

# use ccache
#export PATH="/usr/lib/ccache/bin:$PATH"

export PATH="$PATH:/opt/cuda/bin"

# export PATH="$PATH:/home/vitalyr/projects/dev/cpp/llvm-project/build/bin"
export PATH="$PATH:/home/vitalyr/projects/dev/cpp/circt/build/bin"

# for jshell
export PATH="$PATH:/usr/lib/jvm/default/bin"

# >>> add google depot_tools configuration >>>
export PATH="$PATH:$HOME/sdk/app/depot_tools"
# <<< add google depot_tools configuration <<<

# for firefox to use nvidia va-api
export MOZ_DISABLE_RDD_SANDBOX=1

# for lean
#export PATH="$PATH:$HOME/.elan/toolchains/leanprover--lean4---nightly/bin"

# for intel oneapi
# export PATH="$PATH:/opt/intel/oneapi/compiler/latest/linux/bin:/opt/Xilinx/Vivado/2022.2/bin/"

export DISABLE_QT5_COMPAT=1

export PATH="$PATH:$HOME/.local/share/gem/ruby/3.0.0/bin:/usr/bin/vendor_perl/"

# for the rust crate mlir-sys
export MLIR_SYS_160_PREFIX=/usr/local/opt/llvm@16

export CUDA_PATH=/opt/cuda
export CUDA_INSTALL_PATH=$CUDA_PATH
export CUDA_ROOT=$CUDA_PATH
export PATH="$PATH:$CUDA_PATH/nsight_compute"

export PATH="$PATH:$HOME/.nix-profile/bin:/opt/miniforge/bin"

export WARP_ENABLE_WAYLAND=1

# export RUSTC_WRAPPER=/usr/bin/sccache
export SCCACHE_DIR=$HOME/.cache/sccache
export SCCACHE_CACHE_SIZE="30G"

export PATH="/usr/local/opt/llvm@latest/bin:$PATH"

export PATH="/usr/local/opt/zig/bin:$PATH"

export JAVA_HOME=/lib/jvm/default/

# for local tools
export PATH="/usr/local/bin:$PATH"

# export PATH="$JAVA_HOME/bin:$PATH"

# use clangd from AUR clangd-opt
# export PATH="/opt/clangd/bin:$PATH"

export OLLAMA_NUM_PARALLEL=4

export NVCC_CCBIN=/usr/bin/g++-13
export CUDAHOSTCXX=$NVCC_CCBIN
export DEBUGINFOD_URLS="https://debuginfod.archlinux.org"
. "$HOME/.cargo/env"

export JAX_DUMP_IR_TO=$XDG_CACHE_HOME/jax

export PATH="/home/vitalyr/.local/share/solana/install/active_release/bin:$PATH"
