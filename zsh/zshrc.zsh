eval "$(/opt/homebrew/bin/brew shellenv)"
typeset -gU path fpath cdpath mailpath
bindkey -e
unsetopt correct
export LANG='en_US.UTF-8'
export LESS='-g -i -M -R -S -w -X -z-4'
export VISUAL="emacsclient -c"
export EDITOR="$VISUAL"
alias e="$VISUAL"
alias v=nvim

# ** ALIASES, ETC. **
alias zrc="$EDITOR ~/.zshrc && source ~/.zshrc"
# personal projects
alias code="cdls ~/code -l"
alias dot="cdls ~/code/dotfiles -l"
alias sicp="cdls ~/code/sicp"
alias site="cdls ~/code/maxdarling.github.io"
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

# api keys: i use macOS keychain for github, openai, etc.
# openAI
if command -v security >/dev/null; then
  # note: might have to keychain access -> access control -> 
  # cmd+shfit+G to add /usr/bin/security
  key=$(security find-generic-password -s openai_api_key -w 2>/dev/null)
  if [[ -n "$key" ]]; then
    export OPENAI_API_KEY="$key"
  fi
fi

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

# ** BELOW: PATH STUFF ONLY **

# pnpm
export PNPM_HOME="$HOME/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# jenv
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"

# Cursor Agent
export PATH="$HOME/.local/bin:$PATH"

# Go
export GOPATH="$HOME/go"
export GOBIN="$GOPATH/bin"
export PATH="$PATH:$GOBIN"

# direnv
eval "$(direnv hook zsh)"

# bun completions
[ -s "/Users/mhd/.bun/_bun" ] && source "/Users/mhd/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
