# Usage: make build
#
# Pass force=t to force publishing all orgmode pages regardless of cache status.

ifeq ($(force),)
force := nil
endif

.PHONY: build
build: build-static build-pages-orgmode

.PHONY: clean
clean:
	mkdir -p build/
	rm -f -r build/*

.PHONY: build-dir
build-dir:
	mkdir -p build/

.PHONY: build-static
build-static: | build-dir
	rsync -av --exclude ".#*" site/ build/

.PHONY: build-pages-orgmode
build-pages-orgmode: | build-dir
	emacs --script publish/publish-org-dir.el -- --force=$(force)
