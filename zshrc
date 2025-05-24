# Powerlevel10k Instant Prompt

# Powerlevel10k's instant prompt makes the shell appear instantly, rendering the UI elements while other startup operations happen in the background.


# [[file:zshrc.org::*Powerlevel10k Instant Prompt][Powerlevel10k Instant Prompt:1]]
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
# Powerlevel10k Instant Prompt:1 ends here

# Basic Setup

# Path to the Oh My Zsh installation and theme selection.


# [[file:zshrc.org::*Basic Setup][Basic Setup:1]]
# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/Users/jefffarr/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )
# Basic Setup:1 ends here

# Shell Behavior Options

# Configuration options that control how ZSH behaves.


# [[file:zshrc.org::*Shell Behavior Options][Shell Behavior Options:1]]
# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"
# Shell Behavior Options:1 ends here

# History Configuration

# Configure how command history is stored and displayed.


# [[file:zshrc.org::*History Configuration][History Configuration:1]]
# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
HIST_STAMPS="yyyy-mm-dd"

# History settings for improved usability
setopt INC_APPEND_HISTORY     # Append to history file as commands are executed
setopt HIST_FIND_NO_DUPS      # Don't display duplicates when searching history
setopt HIST_IGNORE_ALL_DUPS   # Don't save duplicates in history
# History Configuration:1 ends here

# Plugins

# Enable Oh My Zsh plugins to extend functionality.


# [[file:zshrc.org::*Plugins][Plugins:1]]
# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  git                # Git integration and shortcuts
  macos              # macOS-specific commands and functions
  docker             # Docker commands and autocomplete
  fzf                # Fuzzy finder integration
  # Important: fzf-tab must be loaded after fzf but before zsh-autosuggestions
  fzf-tab            # Enhanced tab completion with fzf
  zsh-autosuggestions # Command suggestions based on history
  zsh-syntax-highlighting # Syntax highlighting for commands
  # zsh-vi-mode disabled to prevent key binding conflicts with fzf
)

source $ZSH/oh-my-zsh.sh
# Plugins:1 ends here

# User Configuration

# User-specific settings and preferences.


# [[file:zshrc.org::*User Configuration][User Configuration:1]]
# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# enable edit-command-line to edit commands in vim
#autoload -Uz edit-command-line
#zle -N edit-command-line
# todo bind this to a key, appears to be default to ^x^e when in emacs mode
# User Configuration:1 ends here

# Aliases

# Shorthand commands for frequently used operations.


# [[file:zshrc.org::*Aliases][Aliases:1]]
# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Use exa for enhanced ls with icons and git information
alias l="exa -algh --icons"

# Monitor Time Machine backup activity
alias timemachine-activity="sudo fs_usage -w |grep -i backupd |grep -i fsctl"
# Aliases:1 ends here

# Functions for Time Machine

# Functions to view and monitor Time Machine backup logs.


# [[file:zshrc.org::*Functions for Time Machine][Functions for Time Machine:1]]
# Function to display filtered Time Machine logs
function timemachine-logs() {
    echo teest
    printf '\e[3J' && log show --predicate 'subsystem == "com.apple.TimeMachine"' --info --last 6h | grep -F 'eMac' | grep -Fv 'etat' | awk -F']' '{print substr($0,1,19), $NF}'
}
# Functions for Time Machine:1 ends here

# Media and Audio Functions

# Functions for handling YouTube downloads and audio processing.


# [[file:zshrc.org::*Media and Audio Functions][Media and Audio Functions:1]]
# Download YouTube videos as audio files with metadata
function ytdl() {
  #youtube-dl "https://www.youtube.com/watch?v=$1" -o - | ffmpeg -i pipe: $2.wav
  youtube-dl "www.youtube.com/watch?v=$1" -x --write-thumbnail --audio-format wav --audio-quality 0 --write-description -o "~/Music/Samples/Vintage Obscura/%(title)s - %(id)s.%(ext)s"
  # use id3ed to tag
}

# Sync audio samples to MPC Live device
function syncmpclive() {
  rsync --verbose --progress --partial --ignore-existing --exclude '*.webp' --exclude '*.description' --exclude '*.alp'--exclude '*.json' --exclude '*.webp' --exclude '*.description' --exclude '*.jpg' --exclude '*.zip' --copy-links --recursive Music/Samples/MPCLive /Volumes/MPCLive2
}

# Show differences between local and MPC Live device
function mpclivediff() {
  rsync --exclude '*.description' --exclude '*.alp' --copy-links --dry-run --verbose Music/Samples/MPCLive /Volumes/MPCLive2
}

# Open audio files in iZotope RX9
function open-rx9() {
  open -a "iZotope RX9 Audio Editor" "$@"
}

# Convert audio files to 16-bit format
function ot-convert() {
  # @TODO: reduce volume to prevent clipping, then normalize
  find . -type f -name '*.wav' | xargs -t -I % sh -c 'sox "%" -b 16 outfile.wav; mv outfile.wav "%"'
}
# Media and Audio Functions:1 ends here

# SSH Management Functions

# Functions to manage SSH configurations.


# [[file:zshrc.org::*SSH Management Functions][SSH Management Functions:1]]
# Remove entries from SSH known_hosts file
function clean-known-hosts() {
  sed -i '' -e '"$@"d' ~/.ssh/known_hosts
}
# SSH Management Functions:1 ends here

# Shell Options

# Additional settings to configure shell behavior.


# [[file:zshrc.org::*Shell Options][Shell Options:1]]
# Enable directory stack functionality (pushd, popd)
setopt auto_pushd

# Tab completion settings
# Use cursor keys to navigate completion menu
zstyle ':completion:*' menu select
# Enable live completion
zstyle ':completion:*' completer _expand _complete _ignored
# Case-insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# Enable VI mode (currently commented out)
# set -o vi
# Shell Options:1 ends here

# Theme Configuration

# Settings for the Powerlevel10k theme.


# [[file:zshrc.org::*Theme Configuration][Theme Configuration:1]]
# Power Level 10k Configuration
source /usr/local/opt/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
# Theme Configuration:1 ends here

# Configuration

# Configure how autosuggestions work:


# [[file:zshrc.org::*Configuration][Configuration:1]]
# Configure autosuggestions
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#8a8a8a"
# Use right arrow to accept entire suggestion
# Use forward-word (Alt+f) to accept suggestion up to a point
bindkey "^[[1;3C" forward-word # Alt+Right arrow to accept word
# Configuration:1 ends here

# Important Note for iTerm on macOS

# The proper order of plugins is critical for ensuring fzf-tab works correctly:

# 1. The `fzf` plugin must load first
# 2. The `fzf-tab` plugin must load second
# 3. The `zsh-autosuggestions` plugin must load after fzf-tab

# This ensures that all functionality works together correctly without manual loading.


# [[file:zshrc.org::*Important Note for iTerm on macOS][Important Note for iTerm on macOS:1]]
# The fzf-tab plugin is now properly loaded through the oh-my-zsh plugins array
# By placing it after fzf but before zsh-autosuggestions, we ensure it works correctly
# No manual loading is needed here as long as the plugin order is correct
# Important Note for iTerm on macOS:1 ends here

# Configuration


# [[file:zshrc.org::*Configuration][Configuration:1]]
# Properly initialize the completion system for fzf-tab on iTerm
# This ensures a clean, complete initialization on each session
# Force compinit to ignore insecure directories to prevent warnings
autoload -Uz compinit
if [[ -n ${ZDOTDIR}/.zcompdump(#qN.mh+24) ]]; then
  compinit
else
  compinit -C
fi

# Advanced FZF configuration with zsh-autosuggestions compatibility
# Extended FZF options for better visual presentation
export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border --info=inline --marker='✓' --pointer='▶' --prompt='❯ '"

# Configure CTRL-R for enhanced history search with preview
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:wrap --bind 'ctrl-y:execute-silent(echo -n {2..} | pbcopy)+abort' --header 'Press CTRL-Y to copy command to clipboard'"

# Enhanced file search with syntax highlighting preview
export FZF_CTRL_T_OPTS="--preview 'bat --style=numbers --color=always --line-range :500 {} 2>/dev/null || cat {} 2>/dev/null || echo {} 2>/dev/null'"

# Improved directory navigation with content preview 
export FZF_ALT_C_OPTS="--preview 'ls -la --color=always {} | head -50'"

# Disable automatic completion triggering (prevents conflicts with autosuggestions)
export DISABLE_FZF_AUTO_COMPLETION="false"

# Keep key bindings for CTRL-T, CTRL-R, ALT-C
export DISABLE_FZF_KEY_BINDINGS="false"

# Configure fzf-tab for more visible behavior
# Show dot files
zstyle ':completion:*' special-dirs true

# Set different color for each file type
zstyle ':completion:*:*:*:*:*' list-colors ${(s.:.)LS_COLORS}

# Use the preview window for showing content
# Simple file and directory preview configuration
# These settings follow the recommendation from the fzf-tab README

# Preview directory's content with ls when completing cd
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls -la --color=always $realpath'

# Preview files and directories when completing with ls-related commands
zstyle ':fzf-tab:complete:(ls|exa|l|la|ll|lt|tree):*' fzf-preview 'if [[ -d $realpath ]]; then
    ls -la --color=always $realpath
elif [[ -f $realpath ]]; then
    file --mime-type $realpath | grep -q "^.*/text" && 
        bat --style=numbers --color=always --line-range :200 $realpath 2>/dev/null || 
        (file -b $realpath && ls -la $realpath)
else
    echo "No preview available"
fi'

# Preview file content or ls directory when completing with vim/nvim/emacs/etc
zstyle ':fzf-tab:complete:vim:*' fzf-preview 'bat --style=numbers --color=always --line-range :200 $realpath 2>/dev/null || cat $realpath 2>/dev/null || ls -la --color=always $realpath'
zstyle ':fzf-tab:complete:nvim:*' fzf-preview 'bat --style=numbers --color=always --line-range :200 $realpath 2>/dev/null || cat $realpath 2>/dev/null || ls -la --color=always $realpath'
zstyle ':fzf-tab:complete:nano:*' fzf-preview 'bat --style=numbers --color=always --line-range :200 $realpath 2>/dev/null || cat $realpath 2>/dev/null || ls -la --color=always $realpath'
zstyle ':fzf-tab:complete:emacs:*' fzf-preview 'bat --style=numbers --color=always --line-range :200 $realpath 2>/dev/null || cat $realpath 2>/dev/null || ls -la --color=always $realpath'
zstyle ':fzf-tab:complete:bat:*' fzf-preview 'bat --style=numbers --color=always --line-range :200 $realpath 2>/dev/null || cat $realpath 2>/dev/null || ls -la --color=always $realpath'
zstyle ':fzf-tab:complete:cat:*' fzf-preview 'bat --style=numbers --color=always --line-range :200 $realpath 2>/dev/null || cat $realpath 2>/dev/null || ls -la --color=always $realpath'
zstyle ':fzf-tab:complete:less:*' fzf-preview 'bat --style=numbers --color=always --line-range :200 $realpath 2>/dev/null || cat $realpath 2>/dev/null || ls -la --color=always $realpath'
zstyle ':fzf-tab:complete:head:*' fzf-preview 'bat --style=numbers --color=always --line-range :200 $realpath 2>/dev/null || cat $realpath 2>/dev/null || ls -la --color=always $realpath'
zstyle ':fzf-tab:complete:tail:*' fzf-preview 'bat --style=numbers --color=always --line-range :200 $realpath 2>/dev/null || cat $realpath 2>/dev/null || ls -la --color=always $realpath'

# For all other commands, preview files appropriately 
zstyle ':fzf-tab:complete:*:*' fzf-preview 'if [[ -d $realpath ]]; then
    ls -la --color=always $realpath
elif [[ -f $realpath ]]; then
    file --mime-type $realpath | grep -q "^.*/text" && 
        bat --style=numbers --color=always --line-range :200 $realpath 2>/dev/null || 
        (file -b $realpath && ls -la $realpath)
else
    echo "No preview available"
fi'

# Display command help in preview window for commands
# Try different methods to show command help:
# 1. Run --help if available
# 2. Show man page if available
# 3. Show which path if nothing else works
zstyle ':fzf-tab:complete:(\\|)([^:]#):*' fzf-preview '
  if [[ -n "$word" ]]; then
    if command -v "$word" > /dev/null 2>&1; then
      # First try --help
      "$word" --help 2>/dev/null || man "$word" 2>/dev/null || which "$word"
    else
      # If command not found, try man
      man "$word" 2>/dev/null || echo "No help found for $word"
    fi
  fi'
zstyle ':fzf-tab:complete:systemctl-*:*' fzf-preview 'SYSTEMD_COLORS=1 systemctl status $word'
# Special case for kill completion to show process info
zstyle ':fzf-tab:complete:kill:argument-rest' fzf-preview 'ps --pid=$word -o cmd,pid,%cpu,%mem,user,start,time,stat'

# Switch group using `,` and `.`
zstyle ':fzf-tab:*' switch-group ',' '.'

# Specifically for iTerm on macOS: Ensure continuous-trigger mode is enabled
zstyle ':fzf-tab:*' continuous-trigger 'tab'

# Set fzf-tab to use a different fzf command if needed
# This can help with iTerm compatibility
zstyle ':fzf-tab:*' fzf-command fzf

# Configure preview window size and position with enhanced controls
# Make sure the preview window is visible and large enough
zstyle ':fzf-tab:*' fzf-flags '--preview-window=right:60%:wrap' '--bind=ctrl-space:toggle,tab:accept,shift-tab:toggle+down' '--cycle'

# Special settings for iTerm compatibility
# Use these settings to ensure all features work together properly
export TERM="xterm-256color"

# Fix for fzf-tab not working in some terminals
# This ensures the tab key is correctly bound for fzf-tab
bindkey '^I' fzf-tab-complete

# Tip: Use right arrow to accept autosuggestions
# Use CTRL-R for interactive history search with FZF
# Use CTRL-T for interactive file selection with FZF
# Use ALT-C for interactive directory navigation with FZF
# Use TAB for enhanced completion with fzf-tab
# Configuration:1 ends here

# Syntax Highlighting

# Enable syntax highlighting for commands. Must be loaded after sourcing Oh My Zsh.


# [[file:zshrc.org::*Syntax Highlighting][Syntax Highlighting:1]]
# Source the zsh-syntax-highlighting plugin
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# Syntax Highlighting:1 ends here

# FZF Integration

# Fuzzy finder integration for improved file and history search.


# [[file:zshrc.org::*FZF Integration][FZF Integration:1]]
# Source FZF configuration if it exists
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# FZF Integration:1 ends here

# Node Version Manager

# NVM configuration for managing Node.js versions.


# [[file:zshrc.org::*Node Version Manager][Node Version Manager:1]]
# Node Version Manager configuration
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
# Node Version Manager:1 ends here

# Development Tools Configuration

# Settings for various development tools.


# [[file:zshrc.org::*Development Tools Configuration][Development Tools Configuration:1]]
# Configure gtags to use pygments backend
export GTAGSLABEL=pygments

# Set postgres lib in path
export PATH="/opt/homebrew/opt/libpq/bin:$PATH"

# Add Claude CLI to aliases
#alias claude="/Users/jefffarr/.claude/local/claude"
# Development Tools Configuration:1 ends here
