.PHONY: all clean test

all: src/*.lisp .bundle-libs/bundle.lisp Makefile
	rm -rf dist
	mkdir dist
	ros dump executable pixie.ros -o dist/pixie
#		-enable-compression \
#		-remove-docstrings \
#		-delete-all-packages \
#		-destroy-packages-sbcl \
#		-delete-debug-info \
#		-delete-compiler-information-sbcl
#
qlfile.lock: qlfile
	rm -f qlfile.lock
	qlot install

.bundle-libs/bundle.lisp: qlfile.lock
	rm -rf .bundle-libs
	qlot bundle


clean:
	rm -rf .bundle-libs
	rm -rf build
	rm -rf dist
	rm -rf qlfile.lock
