#!/bin/zsh

DOTFILES_DIR="$HOME/dotfiles"
TARGET_DIR="$HOME"

setopt EXTENDED_GLOB NULL_GLOB

symlink_files() {
    local source_dir=$1
    local target_dir=$2

    for item in $source_dir/{.,}*(D); do
        item_name=$(basename "$item")

        if [[ "$item_name" == ".git" || "$item_name" == "setup.sh" || "$item_name" == "manual" ]]; then
            continue
        fi

        local target="$target_dir/$item_name"
        
        # Skip if the target exists and is not a symlink
        if [[ -e "$target" && ! -L "$target" ]]; then
            echo "Skipping $item_name: Target exists and is not a symlink."
            continue
        fi

        rm -rf "$target"

        ln -s "$item" "$target"
        echo "Symlinked $item to $target."
    done
}

symlink_files "$DOTFILES_DIR" "$TARGET_DIR"

if [[ -d "$DOTFILES_DIR/.config" ]]; then
    mkdir -p "$TARGET_DIR/.config"
    symlink_files "$DOTFILES_DIR/.config" "$TARGET_DIR/.config"
fi
