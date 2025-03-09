status is-interactive; and begin

alias c clear
alias g git
alias d sudo
alias ko pkill
alias ls eza
alias du dust
alias ns nsxiv
alias nv nvim
alias pm pulsemixer
alias cat 'bat -p -P'
alias cp 'xcp -vr'
alias rm 'rm -Ivr'
alias mkdir 'mkdir -pv'
alias mv 'mv -iv'
alias df 'duf -hide special'
alias f 'free -h'
alias fd 'fd -p -i'
alias s 'sudo systemctl'
alias im 'timg -p s'
alias eza 'eza --icons always --git --hyperlink --no-quotes'
alias la 'eza -a'
alias ll 'eza -l'
alias lt 'eza --tree'

fzf --fish | source
zoxide init fish --cmd cd | source
starship init fish | source
direnv hook fish | source

set -U fish_greeting
set -U fish_cursor_insert line
set -U fish_cursor_replace_one underscore
set -U fish_cursor_replace underscore

set -gx PATH ~/.config/scripts $PATH

end
