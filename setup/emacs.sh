
emacsRunConfiguration() {
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
	if ! type emacs 1>/dev/null 2>/dev/null; then
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
		ln -s "$CUSTOM_DIR/.emacs.d" "$HOME/.emacs.d" 
		emacsRunConfiguration
	fi
}
