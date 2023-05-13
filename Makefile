all:
	sbcl --load pixie.asd \
		--eval '(asdf:load-system "pixie")' \
		--eval "(sb-ext:save-lisp-and-die #p\"pixie\" :toplevel #'skin.djha.pixie:main :executable t :compression t)"
