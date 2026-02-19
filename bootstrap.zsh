#!/usr/bin/env zsh
set -e
set -x

# Prerequisites:
# - install xcode

# Manual steps:
# - [after] import iterm profile
# - [after] run :PlugInstall in (neo)vim

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
brew install zsh-autosuggestions
brew install git-delta
brew install eza
brew install pure

#######################
# ZSH
#######################
# link my zshrc
ln -sf "$(pwd)/zsh/zshrc.zsh" ~/.zshrc

#######################
# CURSOR
#######################
ln -sf "$(pwd)/cursor/settings.json" ~/Library/Application\ Support/Cursor/User/settings.json
ln -sf "$(pwd)/cursor/keybindings.json" ~/Library/Application\ Support/Cursor/User/keybindings.json

#######################
# EMACS
#######################
brew tap d12frosted/emacs-plus
brew install emacs-plus@31
ln -sf "$(pwd)/emacs/init.el" ~/.emacs.d/init.el
ln -sf "$(pwd)/emacs/customization.el" ~/.emacs.d/init.el
ln -sF "$(pwd)/emacs/lisp" ~/.emacs.d
ln -sF "$(pwd)/emacs/bookmarks" ~/.emacs.d
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
mkdir -p "$(pwd)/vim/autoload" # curling plug.vim fails if this folder doesn't exist
ln -sF "$(pwd)/vim/autoload" ~/.vim

curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

#######################
# NEOVIM
#######################
brew install nvim
mkdir -p ~/.config
ln -sF "$(pwd)/nvim" ~/.config

curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

#######################
# MISC
#######################
# git
ln -sf "$(pwd)/git/.gitconfig" ~/.gitconfig

# Hammerspoon
brew install hammerspoon
ln -sf "$(pwd)/hammerspoon/init.lua" ~/.hammerspoon/init.lua

# VSCode
# brew install visual-studio-code
ln -sf "$(pwd)/vscode/settings.json" ~/Library/Application\ Support/Code/User/settings.json
ln -sf "$(pwd)/vscode/keybindings.json" ~/Library/Application\ Support/Code/User/keybindings.json

echo "bootstrap complete"
