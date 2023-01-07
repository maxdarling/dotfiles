# Fig pre block. Keep at the top of this file.
[[ -f "$HOME/.fig/shell/bash_profile.pre.bash" ]] && builtin source "$HOME/.fig/shell/bash_profile.pre.bash"

# todo: vim autocmd -- every time I save this file, I run `source ~/.bash_profile`

# test
source ~/.doordash_bash_profile.sh

# personal projects
alias sicp="cd ~/Personal/sicp"
alias alg="cd ~/Personal/algorithm-study"
alias skiena="cd ~/Personal/algorithm-design-manual"
alias hammer="vim ~/.hammerspoon/init.lua"
alias nand="cd ~/Personal/nand2tetris"

# git
alias gp="git push"
alias gpl="git pull"
alias gs="git status"
alias gc="git commit"
alias gb="git branch"
alias gl="git log"
alias gamend="git status && git commit -a --no-edit --amend"
alias gch="git checkout"
alias gmast="git checkout master && git pull"

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
export PERSONAL_GITHUB_TOKEN=<keeping this private...>

# Fig post block. Keep at the bottom of this file.
[[ -f "$HOME/.fig/shell/bash_profile.post.bash" ]] && builtin source "$HOME/.fig/shell/bash_profile.post.bash"
