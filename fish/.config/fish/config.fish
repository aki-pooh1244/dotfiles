############################
#      /`·.¸
#     /¸...¸`:·
# ¸.·´  ¸   `·.¸.·´)
#: © ):´;      ¸  {
# `·.¸ `·  ¸.·´\`·¸)
#     `\\´´\¸.·´
#
############################

# emacs alias
alias estart='emacs --daemon'
alias e='emacsclient -nw'
alias ekill="emacsclient -e '(kill-emacs)'"
alias erestart="ekill && estart"
alias eg='emacsclient -c'

alias vi='/usr/bin/vim'

# cd > ls
function cd
  builtin cd $argv
    ls -a
end

#fzf
set -U FZF_LEGACY_KEYBINDINGS 0
set -U FZF_REVERSE_ISEARCH_OPTS "--reverse --height=100%"

#notifier
set -U __done_min_cmd_duration 5000 # default: 5000 ms
set -U __done_exclude 'git (?!push|pull)'  # default: all git commands, except push and pull. accepts a regex.
set -U __done_notify_sound 1

#golang env
export GOPATH=$HOME/go
#export PATH=$PATH:$GOPATH/bin

##python anaconda now not using
#export PATH="/home/aiwata/anaconda3/bin:$PATH"
#export PATH="/opt/anaconda3/bin:$PATH"


#starship prompt (end line)
starship init fish | source

#absolutely final line
