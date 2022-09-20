# for fcitx5
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export XIM=fcitx
export XIM_PROGRAM=fcitx
export SDL_IM_MODULE=fcitx
export CHROME_EXECUTABLE=google-chrome-stable

# allow nix install proprietary packages
export NIXPKGS_ALLOW_UNFREE=1

# for golang tools
export GOPROXY=https://goproxy.io,direct

# make guake run under wayland
export GUAKE_ENABLE_WAYLAND=1

export XDG_CONFIG_HOME=$HOME/.config
export DOOMDIR=$XDG_CONFIG_HOME/doom

# for wlroots
# yeah nvidia 515.43 driver supports VK_EXT_drm_image_format_modifier
export WLR_RENDERER=vulkan
export WLR_NO_HARDWARE_CURSORS=1

# for nvidia proprietary driver
export LIBVA_DRIVER_NAME=vdpau
export VDPAU_DRIVER=nvidia
export __GLX_VENDOR_LIBRARY_NAME=nvidia
export GBM_BACKEND=nvidia-drm

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
export SDL_VIDEODRIVER=wayland
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

export PATH="/opt/riscv/bin:/home/vitalyr/.nix-profile/bin:/home/vitalyr/.local/share/coursier/bin:$PATH"

# use ccache
export PATH="/usr/lib/ccache/bin:$PATH"

export PATH="$PATH:/opt/cuda/bin"

export PATH="$PATH:/home/vitalyr/projects/dev/cpp/llvm-project/build/bin"
export PATH="$PATH:/home/vitalyr/projects/dev/cpp/circt/build/bin"

# for jshell
export PATH="$PATH:/usr/lib/jvm/default/bin"

# >>> add google depot_tools configuration >>>
export PATH="$PATH:/home/vitalyr/sdk/app/depot_tools"
# <<< add google depot_tools configuration <<<
