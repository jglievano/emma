.PHONY: install

install:
	git submodule update --init vendor/magit
	bash -c "cd vendor/magit/ && make"
	ln -s $(shell pwd)/bin/em ~/.local/bin/em
