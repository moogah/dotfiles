# docker aliases
alias dc='docker-compose'

# execute a shell command in a named container
dsh () {
  dc exec $1 /bin/bash
}