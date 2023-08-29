.PHONY: clean
clean:
	mkdir -p build/
	rm -f -r build/*

.PHONY: build
build: build-static build-pages-orgmode

.PHONY: build-dir
build-dir:
	mkdir -p build/

.PHONY: build-static
build-static: | build-dir
	rsync -av --exclude ".#*" site/ build/

.PHONY: build-pages-orgmode
build-pages-orgmode: | build-dir
	emacs --script publish/publish-org-dir.el
