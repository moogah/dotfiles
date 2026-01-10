
# /Users/jefffarr/src/dotfiles/oh-my-zsh/custom/ansible.zsh
#alias ansible-run-role="ansible localhost -m include_role -a name="
ansible-run-role() {
  ansible localhost -m include_role -a name="$@"
}
