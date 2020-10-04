# Git Aliases

alias gs="git status"
alias gd="git diff"

alias ga="git add"
alias gc="git commit -am"
alias gp="git push origin $(git rev-parse --abbrev-ref HEAD)"

alias gcm="git checkout master"
alias gcp="git checkout -"
alias gb="git checkout -b"

alias gpl="git pull"
alias gfrm="git fetch --all && git rebase origin/master"
alias gsa="git submodule add"


# list all branches, sorted by last modified
alias git-history="git for-each-ref --sort='-authordate' --format='%(authordate)%09%(objectname:short)%09%(refname)' refs/heads | sed -e 's-refs/heads/--'"

# Add all files and commit
alias git-commit-all="git add -A && git commit -m"
alias git-ammend="git add -A && git commit --amend --no-edit"

