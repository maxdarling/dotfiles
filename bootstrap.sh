#!/usr/bin/env zsh
set -e
set -x

#######################
# UTILS
#######################

# FZF
brew install fzf

# DIFF-SO-FANCY
brew install diff-so-fancy
# see repo for git color setup instructions
# https://github.com/so-fancy/diff-so-fancy

#######################
# ZSH
#######################

# standard zprezto setup
if [[ ! -d $HOME/.zprezto ]]; then
    git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"

    setopt EXTENDED_GLOB
    for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
      ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
    done
fi

cd $HOME/.zprezto
git pull
git submodule update --init --recursive
cd - 

# use this .zshrc and put a symlink to it in the runcoms dir.
ln -sf "$(pwd)/zsh/zshrc" "${ZDOTDIR:-$HOME}/.zprezto/runcoms/zshrc"


#######################
# NEOVIM
#######################
ln -sF "$(pwd)/nvim" ~/.config

#######################
# VIM
#######################
ln -sf "$(pwd)/vim/vimrc" ~/.vim/vimrc
ln -sf "$(pwd)/vim/base_config.vim" ~/.vim/base_config.vim
ln -sf "$(pwd)/vim/notes.vim" ~/.vim/notes.vim

ln -sF "$(pwd)/vim/plugin" ~/.vim
ln -sF "$(pwd)/vim/pack" ~/.vim
ln -sF "$(pwd)/vim/ftplugin" ~/.vim
ln -sF "$(pwd)/vim/autoload" ~/.vim

#######################
# MISC
#######################
# IntelliJ
ln -sf "$(pwd)/intellij/ideavimrc" ~/.ideavimrc

# Hammerspoon
ln -sf "$(pwd)/hammerspoon/init.lua" ~/.hammerspoon/init.lua

# VSCode
ln -sf "$(pwd)/vscode/settings.json" ~/Library/Application\ Support/Code/User/settings.json
ln -sf "$(pwd)/vscode/keybindings.json" ~/Library/Application\ Support/Code/User/keybindings.json

# Emacs
ln -sf "$(pwd)/emacs/init.el" ~/.emacs.d/init.el


echo "bootstrap complete"
