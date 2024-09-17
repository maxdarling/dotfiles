#!/usr/bin/env zsh
set -e
set -x

# Todo:
# - GET TERM COMPLETION WORKING???

# Prerequisites:
# - install xcode

# Manual steps:
# - [after] import iterm profile

#######################
# system
#######################
if ! command -v brew &> /dev/null
then
    echo "installing homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

brew install go
go install golang.org/x/tools/gopls@latest
brew install pandoc

#######################
# term
#######################
brew install iterm2
brew install font-hack-nerd-font
brew install fzf
brew install diff-so-fancy # see repo for git diff setup instructions

#######################
# ZSH
#######################
# standard zprezto setup
if [[ ! -d $HOME/.zprezto ]]; then
    git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"

    setopt EXTENDED_GLOB
    for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
      ln -sf "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
    done
fi

cd $HOME/.zprezto
git pull
git submodule update --init --recursive
cd - 

# link my zshrc
ln -sf "$(pwd)/zsh/zshrc.zsh" ~/.zshrc

#######################
# EMACS
#######################
brew install emacs-plus
ln -sf "$(pwd)/emacs/init.el" ~/.emacs.d/init.el
ln -sF "$(pwd)/emacs/lisp" ~/.emacs.d
ln -sF "$(pwd)/emacs/bookmarks" ~/.emacs.d/bookmarks
ln -sF "$(pwd)/emacs/snippets" ~/.emacs.d

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

curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

#######################
# NEOVIM
#######################
brew install nvim
ln -sF "$(pwd)/nvim" ~/.config

#######################
# MISC
#######################
# git
ln -sf "$(pwd)/git/.gitconfig" ~/.gitconfig

# IntelliJ
brew install intellij-idea-ce
ln -sf "$(pwd)/intellij/ideavimrc" ~/.ideavimrc

# Hammerspoon
brew install hammerspoon
ln -sf "$(pwd)/hammerspoon/init.lua" ~/.hammerspoon/init.lua

# VSCode
brew install visual-studio-code
ln -sf "$(pwd)/vscode/settings.json" ~/Library/Application\ Support/Code/User/settings.json
ln -sf "$(pwd)/vscode/keybindings.json" ~/Library/Application\ Support/Code/User/keybindings.json

echo "bootstrap complete"
