export CDPATH=.:~:~/src:~/src/norns_apps:~/src/homelab

# https://www.commandlinefu.com/commands/view/8990/generate-an-xkcd-936-style-4-word-password
# create a 4 word password ie: soviethamsterpastaberry 
alias xkcdpass='shuf -n4 /usr/share/dict/words | tr -d '\n''

function append_alias_to_file {
  echo \\n\# $3\\nalias $1=\"$2\" >> $cmd
  source $cmd
}

function add-alias {
  read -A strarr <<< $2

  dotfile-path = "$HOME/src/dotfiles/dotbot"

  cmd="$HOME/.oh-my-zsh/custom/${strarr[1]}.zsh"

  if [ -d $dotfile-path ]; then
    cmd="$dotfile-path/oh-my-zsh/custom/${strarr[1]}.zsh"
  fi

  if [ -f "$cmd" ]; then
    echo "$cmd config file exists."
    append_alias_to_file $1 $2 $3 $cmd
  else
    echo "$cmd config file doesn't exist, creating it."
    touch $cmd
    append_alias_to_file $1 $2 $3 $cmd
  fi

  if [ -d $dotfile-path ]; then
    # re-run dotbot to install the new file
  fi
}