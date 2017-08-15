.PHONY: setup

setup:
	git submodule udpate --init --recursive
	cd vendor/magit/ && make
