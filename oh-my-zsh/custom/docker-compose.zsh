# docker aliases
alias dc='docker-compose'

# execute a shell command in a named container
dsh () {
  dc exec $1 /bin/bash
}
# /Users/jefffarr/src/dotfiles/oh-my-zsh/custom/docker-compose.zsh
alias dcb="docker-compose build"

# /Users/jefffarr/src/dotfiles/oh-my-zsh/custom/docker-compose.zsh
alias dcr="docker-compose run"

# /Users/jefffarr/src/dotfiles/oh-my-zsh/custom/docker-compose.zsh
alias dcrm="docker-compose run --rm"
