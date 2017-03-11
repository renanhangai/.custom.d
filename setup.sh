#!/bin/sh

CUSTOM_DIR=$(dirname $(readlink -f "$0"))

mkdir -p $CUSTOM_DIR/.tmp

for file in $CUSTOM_DIR/setup/*.sh
do
	basename=$(basename $file);
	basename="${basename%.*}";

	. $file

	echo ""
	echo "==============================================="
    echo "Setupping ${basename} "
	echo "==============================================="
    eval ${basename}Setup;
done

echo "Successfully setupped everything"
