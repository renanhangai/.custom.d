#!/bin/sh

CUSTOM_DIR=$(dirname $(readlink -f "$0"))

for file in $CUSTOM_DIR/setup/*.sh
do
	basename=$(basename $file);
	basename="${basename%.*}";

	. $file
    eval ${basename}Setup;
done

echo "Successfully setupped everything"
