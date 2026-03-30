# note: this file is for interactive term features.
# .zprofile contains non-interactive stuff (e.g. PATH)
# (did this 3/29/26 so emacs exec-path-from-shell starts faster)
typeset -gU path fpath cdpath mailpath
bindkey -e
unsetopt correct
alias e="$VISUAL"
alias v=nvim

# ** ALIASES, ETC. **
alias zrc="$EDITOR ~/.zshrc && source ~/.zshrc"
alias co="codex"
# personal projects
alias code="cdls ~/code -l"
alias dot="cdls ~/code/dotfiles -l"
alias pico8="/Applications/PICO-8.app/Contents/MacOS/pico8 -root_path ~/code/pico8"

# git
alias gp="git push -u origin"
alias gpl="git pull"
alias gs="git status"
alias gc="git commit"
alias gb="git branch"
alias gl="git log"
alias gamend="git status && git commit -a --no-edit --amend"
alias gch="git checkout"
alias gmast="git checkout master && git pull origin master"
alias gm="git checkout master && git pull origin master"
alias grc="git rebase --continue"
alias gd="git diff"
alias gde='emacsclient -ce "(magit-diff-unstaged)"'
alias gemp="git commit -m \"empty commit\" --allow-empty"

# personal bash
# alias ls="eza --icons --group-directories-first"
alias ls="eza --group-directories-first"
alias ll="ls -lah"
cdc() { mkdir -p "$1" && cd "$1" }
cdls() { cd "$1" && ls "${@:2}"; }

killports() {
    for port in "$@"
    do
        kill -9 $(lsof -t -i:$port)
    done
}

# github: i use 'gh auth login'

# for typing practice (can paste into monkeytype)
generate_random_words () {
    cat /usr/share/dict/words | awk 'length($0) > 6' | shuf | head -n 1000
}

# torrent
ipv6() {
  # care: no ethernet handling
  [[ "$1" == "0" ]] && sudo networksetup -setv6off Wi-Fi
  [[ "$1" == "1" ]] && sudo networksetup -setv6automatic Wi-Fi
}

# ** TERM STUFF **
# zsh prompt
autoload -U promptinit; promptinit
# hacks to get single-line prompt
prompt_newline=$'\u200B'   # zero-width space (or: prompt_newline='%666v')
PROMPT=" $PROMPT"
prompt pure

# zsh completion
autoload -Uz compinit
compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# zsh autosuggestions
source "$(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'
ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# fzf
source <(fzf --zsh)

# direnv
if [[ -o interactive ]]; then
  eval "$(direnv hook zsh)"

  # bun completions
  [ -s "/Users/mhd/.bun/_bun" ] && source "/Users/mhd/.bun/_bun"
fi
