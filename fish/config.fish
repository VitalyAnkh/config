set fish_greeting
# if test -e /home/vitalyr/.nix-profile/etc/profile.d/nix.fish; . /home/vitalyr/.nix-profile/etc/profile.d/nix.fish; end # added by Nix installer

# include config.d/*

zoxide init fish | source
# mcfly init fish | source

source $HOME/.profile

eval (opam env)
set -gx WAKATIME_HOME "$HOME/.wakatime"

#string match -r ".wasmtime" "$PATH" > /dev/null; or set -gx PATH "$WASMTIME_HOME/bin" $PATH
abbr -a -- am ambient
abbr -a -- soneapi "bash -c 'source /opt/intel/oneapi/setvars.sh ; exec fish'"
abbr -a -- snvhpc "bash -c 'source /etc/profile.d/nvhpc.sh ; exec fish'"
abbr -a -- q QT_QPA_PLATFORM= # imported from a universal variable, see `help abbr`
abbr -a -- ls lsd
abbr -a -- qtx 'env QT_QPA_PLATFORM=xcb'
abbr -a -- pqri 'proxychains -q rua install'
abbr -a -- pmf 'http_proxy=socks5://127.0.0.1:1080 https_proxy=socks5://127.0.0.1:1080 moonfm --proxy-server="socks5://127.0.0.1:1080"'
abbr -a -- qidea QT_QPA_PLATFORM=\'\'\ _JAVA_AWT_WM_NONREPARENTING=1\ idea
abbr -a -- mma env\ QT_QPA_PLATFORM=\'\'\ mathematica
abbr -a -- ped 'http_proxy=socks5://127.0.0.1:1080 https_proxy=socks5://127.0.0.1:1080 element-desktop --proxy-server="socks5://127.0.0.1:1080"'
abbr -a -- lla 'lsd -la'
abbr -a -- ec 'emacsclient --create-frame --alternate-editor=""'
abbr -a -- ll 'lsd -l'
abbr -a -- xtg set\ http_proxy\ http://127.0.0.1:1080\;\ set\ https_proxy\ http_proxy\;\ echo\ \"HTTP\ Proxy\ on\"\;\ env\ QT_QPA_PLATFORM=\'\'\ telegram-desktop
abbr -a -- e LANG=en_US.UTF8
abbr -a -- pqp 'proxychains -q paru'
abbr -a -- sp 'set socks_proxy socks5://127.0.0.1:1080; set http_proxy http://127.0.0.1:8118; set https_proxy $http_proxy; echo "Socks5 Proxy on";'
abbr -a -- doom '$HOME/.config/.emacs.d/bin/doom'
abbr -a -- pqd 'proxychains -q $HOME/.config/.emacs.d/bin/doom'
abbr -a -- pdc 'http_proxy=socks5://127.0.0.1:1080 https_proxy=socks5://127.0.0.1:1080 discord --proxy-server="socks5://127.0.0.1:1080"'
abbr -a -- rle 'set PATH (string match -v /home/vitalyr/projects/dev/cpp/llvm-project/build/bin $PATH)'
abbr -a -- eqqp env\ QT_QPA_PLATFORM=\\\'\\
abbr -a -- pqgp 'proxychains -q git pull'
abbr -a -- ex exercism
abbr -a -- firefox-developer-edition 'env MOZ_ENABLE_WAYLAND=1 firefox-developer-edition'
abbr -a -- ff 'env MOZ_ENABLE_WAYLAND=1 firefox-developer-edition'
abbr -a -- eman 'LANG=en_US.UTF8 man'
abbr -a -- hp 'http_proxy=socks5://127.0.0.1:1080 https_proxy=socks5://127.0.0.1:1080'
abbr -a -- rm 'echo "This is not the command you are looking for."; false'
abbr -a -- pzl 'http_proxy=socks5://127.0.0.1:1080 https_proxy=socks5://127.0.0.1:1080 zulip --proxy-server="socks5://127.0.0.1:1080"'
abbr -a -- setproxy 'set socks_proxy socks5://127.0.0.1:1080; echo "Socks5 Proxy on";'
abbr -a -- tg telegram-desktop
abbr -a -- usp 'set -e socks_proxy; echo "Socks Proxy off";'
abbr -a -- rm_llvm_env 'set PATH (string match -v /home/vitalyr/projects/dev/cpp/llvm-project/build/bin $PATH)'
abbr -a -- rm_nix_env 'set PATH (string match -v /home/vitalyr/.nix-profile/bin $PATH)'
abbr -a -- chez chez-scheme
abbr -a -- tp trash-put
abbr -a -- set_llvm_env 'set PATH /home/vitalyr/projects/dev/cpp/llvm-project/build/bin $PATH'
abbr -a -- unsetproxy 'set -e socks_proxy; echo "Socks Proxy off";'
abbr -a -- pc proxychains
abbr -a -- gl 'git pull'
abbr -a -- pq 'proxychains -q'
abbr -a -- sle 'set PATH /home/vitalyr/projects/dev/cpp/llvm-project/build/bin $PATH'
abbr -a --position anywhere -- V '--version'
abbr -a -- pqo "pacman -Qo"
abbr -a -- pql "pacman -Ql"
abbr -a -- pqi "pacman -Qi"
abbr -a -- pfio "platformio"
abbr -a -- gst "git status"
abbr -a -- grv "git remote -v"
abbr -a -- gtp "git pull"
