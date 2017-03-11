
eslintSetup() {
	echo ""
	echo "==============================================="
    echo "eslintrc"
	echo "==============================================="
	if [ -f "$HOME/.eslintrc" ]; then
		echo ".eslintrc already exists"
	else
		ln -s "$CUSTOM_DIR/.eslintrc" "$HOME/.eslintrc"
		echo "Created link to .eslintrc"
	fi
}

