# echo -ne "\033]0; YOUR_WINDOW_TITLE_HERE \007"

starship init fish | source

#[ -f "/home/arthur/.ghcup/env" ] && source "/home/arthur/.ghcup/env"

# Remove "Welcome to fish [...]" message
set fish_greeting

source "/home/arthur/.config/fish/keys.fish"

export PATH="$HOME/.local/bin:$PATH"
cd

# ghcup-env
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -f /home/arthur/.ghcup/env ; and set -gx PATH $HOME/.cabal/bin /home/arthur/.ghcup/bin $PATH

# Alternative (blocks terminal for 0-3ms)
# cat ~/.cache/wal/sequences

# To add support for TTYs this line can be optionally added.
# bash ~/.cache/wal/colors-tty.sh

