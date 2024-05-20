# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="zhann"

zstyle ':omz:update' mode reminder  # just remind me to update when it's time

ENABLE_CORRECTION="true"

COMPLETION_WAITING_DOTS="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh

alias dotfiles='/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME'

# pnpm
export PNPM_HOME="/home/race/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

[ -f "/home/race/.ghcup/env" ] && . "/home/race/.ghcup/env" # ghcup-env

export FLYCTL_INSTALL="/home/race/.fly"
export PATH="$FLYCTL_INSTALL/bin:$PATH"

alias mydocker='docker build -t mydocker . && docker run --cap-add="SYS_ADMIN" mydocker'

date
