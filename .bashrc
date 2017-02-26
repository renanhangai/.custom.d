#!/bin/sh

# Variables
HOST='\h'
MYGIT_PS1=''
COLORS=$(tput colors)

colorize() {
	if [ -n "$COLORS" ]; then
		echo "$2"
		return
	fi
	
	echo "\[$1\]$2\[\033[00m\]"	
}

#-----------------------
# Git completion if exists
#-----------------------
if [ -n "$(type -t git)" ]; then
    # Try load from hard coded files if not exist
    if [ "$(type -t __git_ps1)" != "function" ]; then
		if [ -f /usr/share/git-core/contrib/completion/git-prompt.sh ]; then
			. /usr/share/git-core/contrib/completion/git-prompt.sh
		fi
    fi
    # Set completition
    if [ -n "$(type -t __git_ps1)" ]; then
		GIT_PS1_SHOWDIRTYSTATE=1
		GIT_PS1_SHOWUPSTREAM=1
		MYGIT_PS1='$(__git_ps1)'
    fi
fi;

# Setup emacs as editor
if [ -n "$(type -t emacs)" ]; then
    export EDITOR="emacs -nw";
    alias emacs="\emacs -nw";
fi

# Alias
alias s="git status -s"


PS1_DIR=$(colorize '\033[01;34m' '\w')

PS1='\n${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@'"$HOST"'\[\033[00m\]:\[\033[01;34m\]\w\[\033[0;34m\]'"$MYGIT_PS1"'\[\033[00m\]\n\[\033[0;32m\]\$\[\033[00m\] '
