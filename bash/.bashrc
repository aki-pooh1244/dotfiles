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
case `uname -a` in
	Darwin* )
	source /usr/local/share/bash-completion/completions/*
	source /usr/local/etc/bash_completion.d/*
	;;
	Linux* )
	source /usr/share/bash-completion/bash_completion
	;;
esac

## setup for wsl
if [[ "$(uname -r)" == *microsoft* ]]; then
	alias open="pwsh.exe /c start"
    	eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
    	export PATH="$PATH:/home/linuxbrew/.linuxbrew/bin"
fi

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
        ls
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
alias ls='ls --color=auto --show-control-chars --time-style=long-iso --human-readable --classify -FH'
alias la='ls -A'
alias ll='ls -l'
alias lsm='ls -hlAG'
# alias tree='exa -T'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias less='less -iRX'
alias cd='cd -P && ls'
# alias up='cd ..'
alias grep='grep --color'

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

## tmux
if which tmux >/dev/null 2>&1; then
    #if not inside a tmux session, and if no session is started, start a new session
    test -z "$TMUX" && (tmux attach || tmux new-session)
fi

## screen
#if [ $TERM != "screen" ]; then
#	if [ $TERM != "dumb" ]; then
#		screen -e ^z^z
#	fi
#fi

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
