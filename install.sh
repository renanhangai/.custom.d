#!/bin/sh

CUSTOM_DIR=${CUSTOM_DIR:-"$HOME/.custom.d"}

mkdir -p $CUSTOM_DIR
wget -O - https://github.com/renanhangai/.config.d/archive/master.tar.gz | tar -xzf - --strip-components=1 -C "$CUSTOM_DIR"
sh "$CUSTOM_DIR/setup.sh"

