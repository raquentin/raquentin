# zsh
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="lamb"
source $ZSH/oh-my-zsh.sh
zstyle ':omz:update' mode reminder
COMPLETION_WAITING_DOTS="true"
plugins=(git)

# gcp
#source 'Users/race.williams/google-cloud-sdk/path.zsh.inc'
#source 'Users/race.williams/google-cloud-sdk/completion.zsh.inc'

# golang
export PATH=$PATH:/usr/local/go/bin

alias ls="eza"
alias cd="z"
alias cat="bat"
alias lg="lazygit"
alias q="exit"
eval "$(zoxide init zsh)"

[ -f "/home/rwilliams/.ghcup/env" ] && . "/home/rwilliams/.ghcup/env" # ghcup-env
