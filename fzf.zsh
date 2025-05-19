# Setup fzf
# ---------
if [[ ! "$PATH" == */Users/jefffarr/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/Users/jefffarr/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/Users/jefffarr/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/Users/jefffarr/.fzf/shell/key-bindings.zsh"

# Default command to use when input is tty
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git 2>/dev/null || find . -type f -not -path "*/\.git/*" -not -path "*/node_modules/*"'
