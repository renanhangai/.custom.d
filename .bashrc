#!/bin/sh

# Custom dir
CUSTOM_DIR=$(dirname "${BASH_SOURCE[0]}")
CUSTOM_DIR=${CUSTOM_DIR:-"$HOME/.custom.d/"}

# Variables
PS1_LOCAL=${CUSTOM_PS1_LOCAL:-'\u@\h'}
PS1_SIGN='$'
if [ "$(id -u)" = "0" ]; then
	PS1_COLOR="31m"
	PS1_LOCAL="ROOT@\H"
	PS1_SIGN='!!!'
elif [ -n "$SSH_CLIENT" ]; then
	PS1_COLOR=${CUSTOM_PS1_COLOR:-"36m"}
else
	PS1_COLOR=${CUSTOM_PS1_COLOR:-"32m"}
fi
COLORS=$(tput colors)

#
# Colorize PS1 output
# Usage
#    colorizePS1 color str
#
colorizePS1() {
	if [ ! -n "$2" ]; then
		return
	fi
	if [ ! -n "$COLORS" ]; then
		echo "$2"
		return
	fi
	echo "\[$1\]$2\[\033[00m\]"	
}

#
# Git completion if exists
#
if type git 1>/dev/null 2>/dev/null; then
    # Try load from hard coded files if not exist
    if ! type __git_ps1 1>/dev/null 2>/dev/null; then
		. "$CUSTOM_DIR/scripts/git-prompt.sh"
    fi
    # Set completition
    if type __git_ps1 1>/dev/null 2>/dev/null; then
		GIT_PS1_SHOWDIRTYSTATE=1
		GIT_PS1_SHOWUPSTREAM=1
	    PS1_GIT='$(__git_ps1)'
    fi

	# Git alias
	alias s="git status -s"
fi

# Setup emacs as editor
if type emacs 1>/dev/null 2>/dev/null; then
    export EDITOR="emacsclient -t -a ''";
    alias emacs="emacsclient -t -a ''";
fi

# Setup the PS1
PS1_LOCAL=$(colorizePS1 '\033[01;$PS1_COLOR' "$PS1_LOCAL")
PS1_DIR=$(colorizePS1 '\033[01;34m' '\w')
PS1_GIT=$(colorizePS1 '\033[0;34m' "$PS1_GIT")
PS1_SIGN=$(colorizePS1 '\033[0;${PS1_COLOR:-32m}' "$PS1_SIGN")
PS1="\n$PS1_LOCAL:$PS1_DIR$PS1_GIT\n$PS1_SIGN "

