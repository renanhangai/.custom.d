
sourcecodeInstall() {
	echo "Downloading source code pro";
	url="https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.tar.gz"

	rm -rf "$CUSTOM_DIR/.tmp/source-code-pro"
	mkdir -p "$CUSTOM_DIR/.tmp/source-code-pro"

	error=1
	if [ -x "$(command -v curl)" ]; then
		curl -L "$url" | tar -xzf - --strip-components=1 -C "$CUSTOM_DIR/.tmp/source-code-pro";
		error=0
	elif [ -x "$(command -v wget)" ]; then
		wget -qO - "$url" | tar -xzf - --strip-components=1 -C "$CUSTOM_DIR/.tmp/source-code-pro";
		error=0
	else
		echo "Please install curl or wget.";
	fi
	
	if [ $error -eq 0 ]; then
		error=1
		if [ "$(id -u)" -eq "0" ]; then
			if [ ! -d "/usr/share/fonts" ]; then
				echo "/usr/share/fonts/ not found";
			else
				mkdir -p "/usr/share/fonts/opentype";
				mv "$CUSTOM_DIR/.tmp/source-code-pro/OTF" "/usr/share/fonts/opentype/source-code-pro";
				error=0;
			fi
		else
			mkdir -p "$HOME/.fonts/opentype";
			mv "$CUSTOM_DIR/.tmp/source-code-pro/OTF" "$HOME/.fonts/opentype/source-code-pro";
			error=0;
		fi;
	fi
	
	if [ $error -ne 0 ]; then
		echo "Error installing script";
	else
		fc-cache -f;
	fi
}

sourcecodeSetup() {
	needinstall=0
	if [ -x "$(command -v fc-list)" ]; then
		echo "Checking font";
		if fc-list | grep -qi "source code pro"; then
			echo "Source code pro already installed";
		else
			needinstall=1
		fi
	else
		echo "No font utility";
	fi
	
	if [ $needinstall -ne 0 ]; then
		while true; do
			read -p "Do you wish to install Source Code Pro [y/N]? " yn
			case $yn in
				[Yy]* ) sourcecodeInstall; break;;
				"" ) break;;
				[Nn]* ) break;;
				* ) echo "Please answer yes or no.";;
			esac
		done
	fi
}
