# zsh
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="lambda"
source $ZSH/oh-my-zsh.sh
zstyle ':omz:update' mode reminder
COMPLETION_WAITING_DOTS="true"
plugins=(git)

# gcp
#source 'Users/race.williams/google-cloud-sdk/path.zsh.inc'
#source 'Users/race.williams/google-cloud-sdk/completion.zsh.inc'

# golang
export GOPATH=$HOME/go
export PATH="/usr/local/opt/go@1.20/bin:$PATH"

alias ls="eza"
alias cd="z"
alias cat="bat"
alias lg="lazygit"
alias q="exit"
alias k8s="kubectl"
eval "$(zoxide init zsh)"
source <(fzf --zsh)
