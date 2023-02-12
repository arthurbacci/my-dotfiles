#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

eval "$(starship init bash)"
export EDITOR=nvim
export OPENSSL_CONF=/dev/null
export GPG_TTY=$(tty)
alias clr="printf '\033[2J\033[3J\033[1;1H'"
export PATH=$PATH:/home/arthur/.local/bin

