.PHONY: install

install:
	git submodule update --init vendor/magit
	bash -c "cd vendor/magit/ && make"
