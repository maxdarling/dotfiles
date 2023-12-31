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
ln -sf "$(pwd)/nvim" ~/.config/nvim


#######################
# VIM
#######################
# todo: replace with loop over all files, minus: [vim/plugged vim/view vim/.netrwhist]
ln -sf "$(pwd)/vim/vimrc" ~/.vim/vimrc

ln -sf "$(pwd)/vim/plugin" ~/.vim/plugin
ln -sf "$(pwd)/vim/pack" ~/.vim/pack
ln -sf "$(pwd)/vim/ftplugin" ~/.vim/ftplugin
ln -sf "$(pwd)/vim/autoload" ~/.vim/autoload

ln -sf "$(pwd)/vim/base_config.vim" ~/.vim/base_config.vim
ln -sf "$(pwd)/vim/coc-settings.json" ~/.vim/coc-settings.json
ln -sf "$(pwd)/vim/notes.vim" ~/.vim/notes.vim


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


echo "bootstrap complete"
