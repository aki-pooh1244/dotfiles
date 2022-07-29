################################################
#     __               __                      #
#    / /_  ____ ______/ /_  __________         #
#   / __ \/ __ `/ ___/ __ \/ ___/ ___/         #
#  / /_/ / /_/ (__  ) / / / /  / /__           #
# /_.___/\__,_/____/_/ /_/_/   \___/           #
################################################

## fzf setup
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
#export FZF_DEFAULT_COMMAND='rg --files --hidden --glob "!.git"'
export FZF_DEFAULT_OPTS='--height 40% --reverse --border'

## bash-completion

source /usr/share/bash-completion/bash_completion
source /usr/local/share/bash-completion/completions/*

## bash option
shopt -s autocd
shopt -s cdspell
shopt -s extglob
shopt -s histappend

## ls color
#if [ "Darwin" = 'Darwin' ]; then
#    alias ls='ls -GFC'
#else
#    alias ls='ls --color=auto -FC'
#fi

## cd -> ls
autols(){
    if [ "$OLDPWD" != "$PWD" ]; then
        exa -F
        OLDPWD="$PWD"
    fi
}
export PROMPT_COMMAND="autols"

## prompt
export PS1="\[$(tput bold)\]\[\033[38;5;207m\]\u\[$(tput sgr0)\]\[\033[48;5;0m\]@\[$(tput sgr0)\]\[\033[38;5;6m\][\[$(tput bold)\]\w\[$(tput sgr0)\]]\[$(tput sgr0)\]\n\[$(tput sgr0)\]\[$(tput bold)\]\[\033[38;5;47m\]>\[$(tput sgr0)\]"

## History
HISTSIZE=1000
HISTFILESIZE=10000

## alias
alias ls='exa -F'
alias la='exa -aF'
alias ll='exa -alF'
alias lsm='exa -hlAFG'
alias tree='exa -T'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
#alias less='less -iRX'
alias cd='cd -P && exa -F'
alias up='cd ..'

alias estart='emacs --daemon'
alias e='emacsclient -nw'
alias ekill="emacsclient -e '(kill-emacs)'"
alias erestart="ekill && estart"
alias eg='emacsclient -c'

alias vi='nano'
alias vim='nano'
## tail
tailc (){
	tail $@ | ccze -A
}
alias tail='tailc'

## less
export LESS='-R'
export LESSOPEN='| /usr/bin/src-hilite-lesspippe.sh %s'

## banjoman/brilliant-bash (https://github.com/banjoman/brilliant-bash.git)
### update: update all of your packages!
#if [ ! -z "$(which brew)" ]; then
#  alias update="brew update && brew upgrade"
#elif [ ! -z "$(which pacman)" ]; then
#  alias update="sudo pacman -Syyu"
#elif [ ! -z "$(which apt)" ]; then
#  alias update="sudo apt update && sudo apt upgrade && sudo apt full-upgrade"
#elif [ ! -z "$(which apt-get)" ]; then
#  alias update ="sudo apt-get update && sudo apt-get upgrade && sudo apt-get dist-upgrade"
#elif [ ! -z "$(which dnf)" ]; then
#  alias update="sudo dnf upgrade"
#elif [ ! -z "$(which yum)" ]; then
#  alias update="su -c 'yum update'"
#elif [ ! -z "$(which zypper)" ]; then
#  alias update="sudo zypper update"
#fi
