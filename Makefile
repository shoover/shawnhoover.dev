.PHONY: clean
clean:
	mkdir -p build/
	rm -f -r build/*

.PHONY: build
build: build-static build-pages-pandoc build-pages-orgmode

.PHONY: build-dir
build-dir:
	mkdir -p build/
	mkdir -p build/pages/pandoc/

.PHONY: build-static
build-static: | build-dir
	rsync -av --exclude ".#*" site/ build/

# Explore generating HTML with pandoc and orgmode in parallel. pandoc is much
# faster than emacs to install in CI. The main drawback is it doesn't rewrite
# org file links. (tbf internal links are also a pain with orgmode.)
PAGES=$(wildcard pages/*.org)
PANDOC_HTML=$(patsubst pages/%.org,build/pages/pandoc/%.html,$(PAGES))
$(PANDOC_HTML): build/pages/pandoc/%.html: pages/%.org | build-dir
	pandoc --standalone -o $@ $<

.PHONY: build-pages-pandoc
build-pages-pandoc: $(PANDOC_HTML)

.PHONY: build-pages-orgmode
build-pages-orgmode: | build-dir
	emacs --script publish/publish-org-dir.el
