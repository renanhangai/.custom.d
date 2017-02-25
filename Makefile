


init.elc: init.el
	emacs --batch --eval '(byte-compile-file "init.el")'
