#!/bin/sh

CUSTOM_DIR=$(dirname $(readlink -f "$0"))

emacsRunConfiguration() {
	echo "Configuring emacs..."
	emacs -q --batch -l "$CUSTOM_DIR/.emacs.d/init.el" --eval="(user-configure)" 2>/dev/null
	echo "Done"
}	


emacsSetup() {
	if [ ! -n "$(type -t emacs)" ]; then
		echo "Emacs is NOT installed. Skipping"
		return
	fi
	
	if [ -d "$HOME/.emacs.d" ]; then
	    EMACS_DIR=$(readlink -f "$HOME/.emacs.d");
		if [ "$EMACS_DIR" = "$CUSTOM_DIR/.emacs.d" ]; then
		    emacsRunConfiguration
		else
			echo "You must delete your previous .emacs.d dir before setupping."
		fi
	else
		ln -s "$HOME/.emacs.d" "$CUSTOM_DIR/.emacs.d"
		emacsRunConfiguration
	fi
}

emacsSetup



