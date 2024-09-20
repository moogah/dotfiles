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
