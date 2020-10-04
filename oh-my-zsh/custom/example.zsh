# You can put files here to add functionality separated per file, which
# will be ignored by git.
# Files on the custom/ directory will be automatically loaded by the init
# script, in alphabetical order.

# For example: add yourself some shortcuts to projects you often work on.
#
# brainstormr=~/Projects/development/planetargon/brainstormr
# cd $brainstormr
#

function my_echo {
  echo $1
}

function append_file {
  echo \\n\# $3\\n#alias $1=$2 >> $cmd
}

function split_string {
  read -A strarr <<< $2

  cmd="$HOME/.oh-my-zsh/custom/${strarr[1]}.zsh"

  echo $cmd

  if [ -f "$cmd" ]; then
    echo "$cmd config file exists."
    append_file $1 $2 $3 $cmd
  else
    echo "$cmd config file doesn't exist, creating it."
    touch $cmd
    append_file $1 $2 $3 $cmd
  fi
}

