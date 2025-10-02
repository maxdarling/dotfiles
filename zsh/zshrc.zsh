# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# disable auto correct
unsetopt correct

# fzf
source <(fzf --zsh)

# settings
export VISUAL=nvim
export EDITOR="$VISUAL"
alias v=nvim # note: 'e' is also aliased to $VISUAL by default...
# export MANPAGER='nvim +Man!'

# mine...
alias zrc="nvim ~/.zshrc && source ~/.zshrc"
alias zpr="nvim ~/.zprezto/runcoms"

alias workrc="nvim ~/.work_bash_profile.sh && source ~/.work_bash_profile.sh && echo 'source completed'"
source ~/.work_bash_profile.sh

# personal projects
alias code="cdls ~/code"
alias dot="cdls ~/code/dotfiles"
alias sicp="cdls ~/code/sicp"
alias alg="cdls ~/code/algorithm-study"
alias skiena="cdls ~/code/algorithm-design-manual"
alias hammer="nvim ~/.hammerspoon/init.lua"
alias nand="cdls ~/code/nand2tetris"
alias site="cdls ~/code/maxdarling.github.io"
alias pico8="/Applications/PICO-8.app/Contents/MacOS/pico8 -root_path ~/code/pico8"
alias maelstrom="~/code/gossip-glomers-sol/maelstrom/maelstrom"

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
alias gemp="git commit -m \"empty commit\" --allow-empty"

cdc() {
    mkdir "$1" && cd "$1"
}

killports() {
    for port in "$@"
    do
        kill -9 $(lsof -t -i:$port)
    done
}

rotate_github_pat() {
  set -euo pipefail
  local user host newpat
  user="${1:-maxdarling}"
  host="${2:-github.com}"

  # prompt securely (not echoed; not in history)
  echo -n "New Pat: "
  read -r -s newpat
  echo

  # erase old entry (if any)
  printf "protocol=https\nhost=%s\nusername=%s\n" "$host" "$user" \
  | git credential-osxkeychain erase || true

  # store new PAT
  printf "protocol=https\nhost=%s\nusername=%s\npassword=%s\n" "$host" "$user" "$newpat" \
  | git credential-osxkeychain store

  echo "PAT rotated in Keychain for ${user}@${host}."
}

# for typing practice (can paste into monkeytype)
generate_random_words () {
    cat /usr/share/dict/words | awk 'length($0) > 6' | shuf | head -n 1000
}

export GOPATH="/opt/homebrew" # ideally I don't have to set this
export PATH="$(go env GOPATH)/bin:$PATH"


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
