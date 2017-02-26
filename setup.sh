#!/bin/sh

CUSTOM_DIR=$(dirname $(readlink -f "$0"))

emacsRunConfiguration() {
	echo "==============================================="
    echo "Emacs "
	echo "==============================================="
	echo "Configuring emacs..."
	emacs -q --batch -l "$CUSTOM_DIR/.emacs.d/init.el" --eval="(user-configure)"
	echo "\nDone"
	echo "Compiling setup..."
	cd "$CUSTOM_DIR/.emacs.d"
	make
	cd "$CUSTOM_DIR"
	echo "\nDone"
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

bashSetup() {
	echo "==============================================="
    echo "bashrc "
	echo "==============================================="
	if [ ! -f "$HOME/.bashrc" ]; then
		echo ".bashrc NOT found"
		return
	fi

	echo "Setupping .bashrc ..."
	if cat "$HOME/.bashrc" | grep -qEz "\#:auto\.custom\.d:.*?\#:~auto\.custom\.d:"; then
		echo ".custom.d already setupped on .bashrc"
		return
	fi
	BASHRC_CONTENT="

#:auto.custom.d: 
if [ -f \"$CUSTOM_DIR/.bashrc\" ]; then
\t# CUSTOM_PS1_LOCAL='\u@\h'
\t# CUSTOM_PS1_COLOR='32m'
\t. \"$CUSTOM_DIR/.bashrc\"
fi
#:~auto.custom.d:"

	echo "$BASHRC_CONTENT" >> "$HOME/.bashrc"
	return
}

emacsSetup
echo ""
bashSetup
echo ""

echo "Successfully setupped everything"
