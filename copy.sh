#!/bin/bash

echo "[y/n] will you remove sensitive info before uploading? (e.g. github api key?)"
read xx
if [[ "$xx" != "y" ]]; then
    echo "received '$xx'. stopping..."
    exit 0
fi

# vim
cp ~/.vimrc .
cp -R ~/.vim/ftplugin .

# bash profile
cp ~/.bash_profile .

# hammerspoon
cp ~/.hammerspoon/init.lua

    
echo "done copying: vim, bash profile, hammerspoon"
