# zsh
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="lamb"
zstyle ':omz:update' mode reminder
COMPLETION_WAITING_DOTS="true"
plugins=(git)
source $ZSH/oh-my-zsh.sh

# golang
export GOPATH=$HOME/go
export PATH="/usr/local/opt/go@1.20/bin:$PATH"

alias ls="eza"
eval "$(zoxide init zsh)"
