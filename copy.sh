#!/bin/bash

# echo "[y/n] will you remove sensitive info before uploading? (e.g. github api key?)"
# read xx
# if [[ "$xx" != "y" ]]; then
#     echo "received '$xx'. stopping..."
#     exit 0
# fi

# vim
if [ ! -d ".vim" ]; then
    mkdir ".vim"
fi
cp ~/.vim/vimrc .vim/vimrc
cp ~/.vim/notes.vim .vim/notes.vim
cp ~/.vim/utils.vim .vim/utils.vim
cp -R ~/.vim/ftplugin .vim
cp -R ~/.vim/plugin .vim
cp -R ~/.vim/pack .vim
cp -R ~/.vim/autoload .vim

# bash profile
cp ~/.bash_profile .
echo "removing PERSONAL_GITHUB_TOKEN from bashrc..."
sed -i '' 's/\(export PERSONAL_GITHUB_TOKEN=\).*/\1\<deleted\>/' ./.bash_profile

# hammerspoon
cp ~/.hammerspoon/init.lua hammerspoon.lua

echo "done copying: vim, bash profile, hammerspoon"
