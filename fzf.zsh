# Path Setup

# Make sure fzf is in your PATH.


# [[file:fzf/fzf.org::*Path Setup][Path Setup:1]]
# Setup fzf
# ---------
if [[ ! "$PATH" == */usr/local/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/usr/local/bin"
fi
# Path Setup:1 ends here

# FZF Base Directory

# Set the base directory to help oh-my-zsh plugin locate fzf.


# [[file:fzf/fzf.org::*FZF Base Directory][FZF Base Directory:1]]
# Set environment variable to help oh-my-zsh plugin locate fzf
export FZF_BASE="/Users/jefffarr/.fzf"
# FZF Base Directory:1 ends here

# Key Bindings Configuration

# Ensure key bindings are enabled.


# [[file:fzf/fzf.org::*Key Bindings Configuration][Key Bindings Configuration:1]]
# Make sure key bindings are enabled (unset the disable flag)
unset DISABLE_FZF_KEY_BINDINGS
# Key Bindings Configuration:1 ends here

# Default Command

# Set the default command that FZF uses to generate the list of files it searches through.
# This uses =fd= if available (with fallback to =find=), which respects .gitignore and is faster.


# [[file:fzf/fzf.org::*Default Command][Default Command:1]]
# Default command to use when input is tty
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git 2>/dev/null || find . -type f -not -path "*/\.git/*" -not -path "*/node_modules/*"'
# Default Command:1 ends here

# Default Options

# Set default UI options for a more pleasant experience.


# [[file:fzf/fzf.org::*Default Options][Default Options:1]]
# Set basic fzf options for better UI
export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border"
# Default Options:1 ends here

# CTRL-R: History Search

# Enhanced history search with preview. This is the critical configuration for reverse-i-search.


# [[file:fzf/fzf.org::*CTRL-R: History Search][CTRL-R: History Search:1]]
# CTRL-R - Focused configuration for reverse history search
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:wrap"
# CTRL-R: History Search:1 ends here

# Direct Key Bindings Source

# Directly source the key bindings file to ensure CTRL-R works properly.


# [[file:fzf/fzf.org::*Direct Key Bindings Source][Direct Key Bindings Source:1]]
# Directly source the key bindings file to ensure CTRL-R works
if [[ -f "/Users/jefffarr/.fzf/shell/key-bindings.zsh" ]]; then
  source "/Users/jefffarr/.fzf/shell/key-bindings.zsh"
  # Explicitly bind CTRL-R to the history widget
  bindkey '^R' fzf-history-widget 2>/dev/null
fi
# Direct Key Bindings Source:1 ends here
