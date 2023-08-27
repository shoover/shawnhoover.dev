.PHONY: clean
clean:
	mkdir -p build/
	rm -f -r build/*

.PHONY: build
build: build-static build-pages-pandoc build-pages-orgmode

.PHONY: build-dir
build-dir:
	mkdir -p build/

.PHONY: build-static
build-static: | build-dir
	rsync -av --exclude ".#*" site/ build/

.PHONY: build-pages-pandoc
build-pages-pandoc: | build-dir
	# pandoc is much faster than emacs to install in CI for generating HTML. The
	# main drawback is it doesn't rewrite org file links. (tbf internal links
	# are also a pain with orgmode.)
	mkdir -p build/pages/pandoc/
	pandoc -o build/pages/pandoc/hello.html --standalone pages/hello.org
	pandoc -o build/pages/pandoc/notes.html --standalone pages/notes.org

.PHONY: build-pages-orgmode
build-pages-orgmode: | build-dir
	emacs --script publish/publish-org-dir.el
