.PHONY: setup

setup:
	git submodule update --init --recursive
	cd vendor/magit/ && make
	ln -s $(shell pwd)/bin/em ~/bin/em
