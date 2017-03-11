bashSetup() {
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
