#!/bin/bash

directories=('vim' 'nvim' 'vscode' 'intellij' 'bash'
    'hammerspoon')
for d in ${directories[@]}; do
    if [[ ! -d $d ]]; then
        mkdir $d 
        echo "created directory $d"
    fi
done

# vim
cp -R ~/.vim/ vim 2>/dev/null
rm -rf vim/plugged vim/view vim/.netrwhist

# nvim
cp -R ~/.config/nvim/ nvim

# VSCode
cp \
~/Library/Application\ Support/Code/User/settings.json \
~/Library/Application\ Support/Code/User/keybindings.json \
vscode

# IntelliJ
cp ~/.ideavimrc intellij

# bash profile
cp ~/.bash_profile bash
echo "removing PERSONAL_GITHUB_TOKEN from bashrc..."
sed -i '' \
's/\(export PERSONAL_GITHUB_TOKEN=\).*/\1\<deleted\>/' \
bash/.bash_profile

# hammerspoon
cp ~/.hammerspoon/init.lua hammerspoon

echo "done copying!"
