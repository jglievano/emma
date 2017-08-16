.PHONY: setup

setup:
	git submodule udpate --init --recursive
	cd vendor/magit/ && make
	ln -s bin/em ~/bin/em
