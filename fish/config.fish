status is-interactive; and begin

fish_vi_key_bindings

abbr -a ts --set-cursor "nix shell --impure nixpkgs#%"
abbr -a rs --set-cursor "nix run --impure nixpkgs#%"
abbr -a np --set-cursor "nix run --impure github:nixos/nixpkgs#%"

alias c clear
alias g git
alias d sudo
alias ls eza
alias du dust
alias ns nsxiv
alias nv 'emacsclient -nw'
alias pm pulsemixer
alias cat 'bat -p -P'
alias ko "pkill -9"
alias up "nh os switch"
alias ss "nh search"
alias rg "rg -S"
alias cp 'xcp -vr'
alias rm 'rm -Ivr'
alias mkdir 'mkdir -pv'
alias mv 'mv -iv'
alias df 'duf -hide special -style ascii'
alias f 'free -h'
alias fd 'fd -p -i --hyperlink'
alias s 'sudo systemctl'
alias fzf 'fzf --color=16'
alias im 'timg -p s'
alias eza 'eza --icons always --git --hyperlink --no-quotes'
alias la 'eza -a'
alias ll 'eza -l'
alias lt 'eza --tree'

fzf --fish | source
zoxide init fish --cmd cd | source
starship init fish | source
direnv hook fish | source
spotify_player generate fish | source

set -U fish_greeting
set -U fish_cursor_insert line
set -U fish_cursor_replace_one underscore
set -U fish_cursor_replace underscore

set -gx DIRENV_LOG_FORMAT
set -gx NIXPKGS_ALLOW_UNFREE 1
set -gx MANPAGER "less -R --use-color -Dd+m -Du+b -DP+g -DE+c -DW+y"
set -gx MANROFFOPT "-P -c"
set -gx LESS "-R --use-color"
set -gx BAT_THEME "base16-256"
set -gx EDITOR "emacsclient -nw"

end
