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

  dotfile="$HOME/src/dotfiles"
  dotbot="${dotfile}/dotbot"
  zshcustom="~/.oh-my-zsh/custom"

  filename="${strarr[1]}.zsh"

  cmd="$HOME/.oh-my-zsh/custom/${filename}"

  if [ -d $dotbot ]; then
    cmd="$HOME/src/dotfiles/oh-my-zsh/custom/${filename}"
  fi

  if [ -f "$cmd" ]; then
    echo "$cmd config file exists."
    append_alias_to_file $1 $2 $3 $cmd
  else
    echo "$cmd config file doesn't exist, creating it."
    touch $cmd
    append_alias_to_file $1 $2 $3 $cmd
  fi

  if [ -d $dotbot ]; then
    echo \\n-\ link\:\\n"    "$zshcustom/$filename\: "oh-my-zsh/custom/${filename}" >> "$HOME/src/dotfiles/install.conf.yaml"
    # re-run dotbot to install the new file
    $dotfile/install --only link 1>/dev/null
  fi
}