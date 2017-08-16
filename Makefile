.PHONY: setup

setup:
	git submodule udpate --init --recursive
	cd vendor/magit/ && make
	ln -s $(shell pwd)bin/em ~/bin/em
