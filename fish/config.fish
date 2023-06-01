# erase the GDK_BACKEND variable
# set -e GDK_BACKEND
set fish_greeting

# TODO: which config set the LD_LIBRARY_PATH variable?
# it shouldn't do this.
# set -e LD_LIBRARY_PATH

source $HOME/.profile

# include config.d/*

zoxide init fish | source
# mcfly init fish | source

eval (opam env)
set -gx WAKATIME_HOME "$HOME/.wakatime"

#string match -r ".wasmtime" "$PATH" > /dev/null; or set -gx PATH "$WASMTIME_HOME/bin" $PATH
