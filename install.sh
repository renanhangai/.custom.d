#!/bin/sh

CUSTOM_DIR=${CUSTOM_DIR:-"$HOME/.custom.d"}

if [ -d "$CUSTOM_DIR" ]; then
	echo "$CUSTOM_DIR already exists. Please delete it or remove";
	exit;
fi

if [ -n "$(type -t git)" ]; then
	git clone https://github.com/renanhangai/.custom.d "$CUSTOM_DIR";
elif [ -n "$(type -t curl)" ]; then
	echo "Downloading file";
	mkdir -p $CUSTOM_DIR;
    curl -L https://github.com/renanhangai/.custom.d/archive/master.tar.gz | tar -xzf - --strip-components=1 -C "$CUSTOM_DIR";
elif [ -n "$(type -t wget)" ]; then
	echo "Downloading file";
	mkdir -p $CUSTOM_DIR;
	wget -qO - https://github.com/renanhangai/.custom.d/archive/master.tar.gz | tar -xzf - --strip-components=1 -C "$CUSTOM_DIR";
else
	echo "Cannot download. Please install curl or wget."
	exit;
fi

echo "\n\n\n\nSetupping .custom.d";
sh "$CUSTOM_DIR/setup.sh";

