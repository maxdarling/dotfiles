# kotlin langauge server (vim)
export PATH="~/lsp/kotlin/kotlin-language-server/server/build/install/server/bin:$PATH"

# todo: vim autocmd -- every time I save this file, I run `source ~/.bash_profile`

# testing
function cd {
    builtin cd "$@" && ls -F
}

# filter out MacOs home dirs from 'ls'. I wish mac came with ls --ignore...
lsh() {
    ls "$@" | grep -Ewv \
        'Music|Pictures|Library|Applications|Desktop|Documents|Movies|Public' 
}

# test: remap fzf ALT-C
export FZF_ALT_C_OPS="--bind 'ctrl-q'"

alias bp="vim ~/.bash_profile && source ~/.bash_profile && echo 'source completed'"
alias ddbp="vim ~/.doordash_bash_profile.sh && source ~/.doordash_bash_profile && echo 'source completed'"
source ~/.doordash_bash_profile.sh

# personal projects
alias dot="cd ~/code/dotfiles"
alias sicp="cd ~/code/sicp"
alias alg="cd ~/code/algorithm-study"
alias skiena="cd ~/code/algorithm-design-manual"
alias hammer="vim ~/.hammerspoon/init.lua"
alias nand="cd ~/code/nand2tetris"

# git
alias gp="git push -u origin"
alias gpl="git pull"
alias gs="git status"
alias gc="git commit"
alias gb="git branch"
alias gl="git log"
alias gamend="git status && git commit -a --no-edit --amend"
alias gch="git checkout"
alias gmast="git checkout master && git pull"
alias grc="git rebase --continue"

# vim 
alias v="vim"
alias vp="vim ~/.vimrc"


bashman () { man bash | less -p "^       $1 "; }

touch2() { mkdir -p "$(dirname "$1")" && touch "$1" ; }

killports() {
    for port in "$@"
    do
        kill -9 $(lsof -t -i:$port)
    done
}

# TOKENS / SECRETS
# example: git push https://maxdarling:${PERSONAL_GITHUB_TOKEN}@github.com/maxdarling/algorithm-design-manual.git
# for repos, do git remote add <above url, tweaked for repo>
export PERSONAL_GITHUB_TOKEN=<deleted>

git_personal_remote() {
    repo="$(pwd | sed -E 's#.*/##')"
    echo "[y/n] is this the repo name?: $repo"
    read yn
    if [[ "$yn" != "y" ]]; then
        echo "exiting"
        kill -INT $$
    fi
    git remote remove origin 2>/dev/null
    str="git remote add origin https://maxdarling:${PERSONAL_GITHUB_TOKEN}@github.com/maxdarling/${repo}.git"
    eval "$str"
    echo "done"
}
