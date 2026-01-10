# Enhanced fzf.zsh with productivity-boosting integrations

# CTRL-R - Enhanced history search with preview and clipboard support
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:wrap --bind 'ctrl-y:execute-silent(echo -n {2..} | pbcopy)+abort' --header 'Press CTRL-Y to copy command to clipboard'"

# CTRL-T - Paste the selected files and directories onto the command line
export FZF_CTRL_T_COMMAND="fd --type f --hidden --follow --exclude .git 2>/dev/null || find . -type f -not -path '*/\.git/*' -not -path '*/node_modules/*'"
export FZF_CTRL_T_OPTS="--preview 'bat --style=numbers --color=always --line-range :500 {} 2>/dev/null || cat {} 2>/dev/null || echo {} 2>/dev/null'"

# ALT-C - cd into the selected directory
export FZF_ALT_C_COMMAND="fd --type d --hidden --follow --exclude .git 2>/dev/null || find . -type d -not -path '*/\.git/*' -not -path '*/node_modules/*'"
export FZF_ALT_C_OPTS="--preview 'ls -la {} | head -50'"

# Default options
export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border --info=inline"

# Interactive git add
fga() {
  git status --short |
  fzf --multi --preview 'git diff --color {+2}' |
  awk '{print $2}' |
  xargs git add
}

# Git checkout branch
fgb() {
  git branch |
  grep -v HEAD |
  sed 's/^..//' |
  fzf --preview 'git log --oneline --graph --date=short --color=always --pretty="format:%C(auto)%cd %h%d %s" {} | head -200' |
  xargs git checkout
}

# Git checkout commit
fgc() {
  git log --oneline --color=always | 
  fzf --ansi --no-sort --preview 'git show --color=always {1}' | 
  awk '{print $1}' | 
  xargs git checkout
}

# Kill process interactively
fkill() {
  local pid
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

  if [ "x$pid" != "x" ]; then
    echo $pid | xargs kill -${1:-9}
  fi
}

# Enhanced cd with preview
fcd() {
  local dir
  dir=$(find ${1:-.} -path '*/\.*' -prune -o -type d -print 2> /dev/null | fzf +m --preview 'ls -la {}')
  cd "$dir"
}

# Search file contents with ripgrep and open in $EDITOR
frg() {
  result=$(rg --line-number --no-heading --color=always --smart-case "${*:-''}" |
    fzf --ansi \
        --color "hl:-1:underline,hl+:-1:underline:reverse" \
        --delimiter : \
        --preview 'bat --color=always {1} --highlight-line {2}' \
        --preview-window 'up,60%,border-bottom,+{2}+3/3,~3')
  
  if [[ -n "$result" ]]; then
    file=$(echo "$result" | awk -F: '{print $1}')
    line=$(echo "$result" | awk -F: '{print $2}')
    
    if [[ -n $EDITOR ]]; then
      $EDITOR "$file" +$line
    else
      vim "$file" +$line
    fi
  fi
}

# Get inside a running Docker container
fdoc() {
  local cid
  cid=$(docker ps | sed 1d | fzf -1 -q "$1" | awk '{print $1}')

  [ -n "$cid" ] && docker exec -it "$cid" /bin/bash
}

# Browse environment variables
fenv() {
  local out
  out=$(env | fzf)
  echo $(echo $out | cut -d= -f2)
}

# Switch tmux session
ftm() {
  [[ -n "$TMUX" ]] && change="switch-client" || change="attach-session"
  if [ $1 ]; then
    tmux $change -t "$1" 2>/dev/null || (tmux new-session -d -s $1 && tmux $change -t "$1"); return
  fi
  session=$(tmux list-sessions -F "#{session_name}" 2>/dev/null | fzf --exit-0) &&  
  tmux $change -t "$session" || echo "No sessions found."
}

# Fuzzy search file paths in Git repo and edit
fgf() {
  git ls-files | fzf --preview 'bat --style=numbers --color=always {}' | xargs -o ${EDITOR:-vim}
}

# Enhanced file search and edit
fe() {
  IFS=$'\n' files=($(fzf-tmux --preview 'bat --style=numbers --color=always {}' --query="$1" --multi --select-1 --exit-0))
  [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
}

# Search and navigate through recent directories
fz() {
  local dir
  dir="$(z -l | sort -rn | awk '{print $2}' | fzf --preview 'ls -la {}')" && cd "$dir"
}

# Find and install homebrew packages
fbrew() {
  local inst=$(brew search | fzf -m)

  if [[ $inst ]]; then
    for prog in $(echo $inst); do
      brew install $prog
    done
  fi
}